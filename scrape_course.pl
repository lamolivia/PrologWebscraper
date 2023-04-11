:- use_module(library(sgml)).
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
:- include(scheduling).
:- use_module(library(dcg/basics)).

string_trim(String, Trimmed) :-
    string_codes(String, Codes),
    phrase((blanks, string(CodesTrimmed), blanks), Codes),
    string_codes(Trimmed, CodesTrimmed).


remove_whitespace_char(Char) :-
    member(Char, [9, 10, 11, 12, 13, 32]).


remove_whitespace(String, AtomResult) :-
    string_to_atom(String, AtomizedString),
    atom_codes(AtomizedString, Codes),
    exclude(remove_whitespace_char, Codes, Result),
    atom_codes(AtomResult, Result).


url(DeptSpaces, CourseNumberSpaces, URL) :-
    remove_whitespace(DeptSpaces, Dept),
    remove_whitespace(CourseNumberSpaces, CourseNumber),
    %  error handling for whitespace added to url
    format(atom(URL), 'https://courses-test.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-course&dept=~w&course=~w', [Dept, CourseNumber]).
    

download_page(URL, HTML) :-
    http_open(URL, In, []),
    load_html(In, HTML, []),
    close(In).

% remove type data from section for easier use in later parts of code
remove_type_from_section(section(SectionName, Start, End, Day, Term, _), section(SectionName, Start, End, Day, Term)).

% create a component that contains all sections of a certain type. I.e. a lab component with all lab sections
make_component(Sections, CourseName, Type, component(CourseName, Type, UnTypeSections)) :-
    include(match_type(Type), Sections, TypeSections),
    maplist(remove_type_from_section, TypeSections, UnTypeSections).

match_type(IntendedType, section(_, _, _, _, _, Type)) :-
    IntendedType = Type.

find_types(Sections, Types) :-
    findall(Type, (
        member(section(_, _, _, _, _, Type), Sections),
        Type \= ""
    ), DupeTypes),
    list_to_set(DupeTypes,Types).

% for a given coursename, finds all components. For example, for CPSC 121, it would return a Lecture, Lab and Tutorial.
extract_data(Html, CourseName, Components) :-
    xpath(Html, //table(contains(@class,'section-summary')), Table),

    % find every section related to the course(all lab sections, lecture sections, etc)
    findall(section(SectionName, Start, End, Day,  Term, Type), (
        extract_section(Table, Type, Start,End,Day,SectionName, Term),
        Term =:= 1 % Only include sections with Term 1
    ), UnsortedDupeSections),

    % find a list of all component types in the course(Lab, lecture, etc.)
    list_to_set(UnsortedDupeSections, UnsortedSections),
    find_types(UnsortedSections, TypesWWaitlist),

    % remove waiting list from types to ensure the algorithm doesnt sign us up for waitlists
    exclude(=("Waiting List"), TypesWWaitlist, Types),

    % aggregate sections into components. all lab sections should be under a Lab component, etc.
    maplist(make_component(UnsortedSections, CourseName), Types, Components).
    

% convert data of format HH:MM to clock_time(HH, MM) as needed for later time comparisons
to_clock_time(Text, clock_time(Hrs, Mins)) :-
    split_string(Text, ":", "", [StrHrs,StrMins]),
    number_string(Hrs, StrHrs),
    number_string(Mins, StrMins).

% extract an individual section of a component. i.e., if the component is a lab, this would extra L2A, L2B, etc.
% finds any 
extract_section(Table, Type, Start, End, Day, SectionName, Term) :-
    % extract data relevant to the section with xpath
    xpath(Table, //tr(contains(@class,'section')), Section),
    xpath(Section, //td(2,text(string)), SectionName),
    xpath(Section, //td(3,text(string)), Type),
    xpath(Section, //td(4,number), Term),
    xpath(Section, //td(7,text(string)), Days), 

    % map days of week to a shorter version
    re_matchsub("Mon|Tue|Wed|Thu|Fri|Sat|Sun", Days, ReDay),
    dict_pairs(ReDay, _, Pairs),
    memberchk(_-Day, Pairs),
    
    xpath(Section, //td(8,text(string)), StartString),
    % convert the time to  a clock_time, needed for later comparisons
    to_clock_time(StartString, Start),
    
    xpath(Section, //td(9,text(string)), EndString),
    % ditto
    to_clock_time(EndString, End).


% save scraped data to a file
save_scrape_to_file(Data) :-
    open('output.txt', write, Out),
    write(Out, Data),
    close(Out).

% save schedule data to a file
save_sched_to_file(Data) :-
    open('schedule.txt', write, Out),
    write(Out, Data),
    close(Out).

% main function to run web scraping
run_web_scraper :-
    % prompt user for courses to register
    writeln("Please input the courses you wish to enrol in, separated by commas (ex. ""CPSC 100, FNH 150""):"),
    read_line_to_string(user_input, Input),
    split_string(Input, ",", " ", ClassList0),
    writeln("List received. Scraping data..."),
    maplist(string_trim, ClassList0, ClassList),
    
    % call the helper on every class name we receieved
    maplist(web_scraper_helper, ClassList, ListOfCourseDataLists),
    % consolidate into one single component list
    foldl(append, ListOfCourseDataLists, [], CourseData),
    save_scrape_to_file(CourseData),
    
    writeln('Data has been scraped. Creating schedule.'),
    start_scheduling(ClassList, CourseData, Schedule),
    save_sched_to_file(Schedule),
    writeln("Finished!"),
    abort.

web_scraper_helper(DeptCourseNum, CourseData) :-
    split_string(DeptCourseNum, " ", "", [Dept, CourseNum]),
    % compute the url of the course were registering
    url(Dept, CourseNum, URL),
    % download the webpage and extract the data into components
    download_page(URL, HTML),
    extract_data(HTML, DeptCourseNum, CourseData),

    % sleep so we dont get ip banned lol
    sleep(5).

