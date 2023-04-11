:- use_module(library(sgml)).
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
:- include(scheduling).


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



remove_type_from_section(section(SectionName, Start, End, Day, Term, _), section(SectionName, Start, End, Day, Term)).

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

extract_data(Html, CourseName, Components) :-
    xpath(Html, //table(contains(@class,'section-summary')), Table),
    % [component(CPSC 001,Lecture,[[section(clock_time(15,00),clock_time(16,00),Mon,CPSC 100 101,1)]])],
    findall(section(SectionName, Start, End, Day,  Term, Type), (
        extract_section(Table, Type, Start,End,Day,SectionName, Term)
    ), UnsortedSections),
    find_types(UnsortedSections, Types),
    maplist(make_component(UnsortedSections, CourseName), Types, Components).
    


to_clock_time(Text, clock_time(Hrs, Mins)) :-
    split_string(Text, ":", "", [StrHrs,StrMins]),
    number_string(Hrs, StrHrs),
    number_string(Mins, StrMins).

extract_section(Table, Type, Start, End, Day, SectionName, Term) :-
    xpath(Table, //tr(contains(@class,'section')), Section),
    xpath(Section, //td(2,text(string)), SectionName),
    xpath(Section, //td(3,text(string)), Type),
    xpath(Section, //td(4,number), Term),
    xpath(Section, //td(7,text(string)), Days), % need to parse this
    re_matchsub("Mon|Tue|Wed|Thu|Fri|Sat|Sun", Days, ReDay),
    dict_pairs(ReDay, _, Pairs),
    memberchk(_-Day, Pairs),
    xpath(Section, //td(8,text(string)), StartString),
    to_clock_time(StartString, Start),
    xpath(Section, //td(9,text(string)), EndString),
    to_clock_time(EndString, End).


save_scrape_to_file(Data) :-
    open('output.txt', write, Out),
    write(Out, Data),
    close(Out).

save_sched_to_file(Data) :-
    open('schedule.txt', write, Out),
    write(Out, Data),
    close(Out).

run_web_scraper :-
    writeln("Please input the courses you wish to enrol in, separated by commas (ex. ""CPSC 100, FNH 150""):"),
    read_line_to_string(user_input, Input),
    %  TODO: add trim spaces, but not a big deal
    split_string(Input, ",", "", ClassList),
    maplist(web_scraper_helper, ClassList, ListOfCourseDataLists),
    foldl(append, ListOfCourseDataLists, [], CourseData),
    %  debugging purposes
    save_scrape_to_file(CourseData),
    % Example = [component(CPSC 001,Lecture,[[section(clock_time(15,00),clock_time(16,00),Mon,CPSC 100 101,1)]])],
    start_scheduling(ClassList, CourseData, Schedule),
    %  debugging purposes
    save_sched_to_file(Schedule),
    % writeln("finished"),
    halt.

web_scraper_helper(DeptCourseNum, CourseData) :-
    split_string(DeptCourseNum, " ", "", [Dept, CourseNum]),
    url(Dept, CourseNum, URL),
    download_page(URL, HTML),
    extract_data(HTML, DeptCourseNum, CourseData),
    sleep(5).

