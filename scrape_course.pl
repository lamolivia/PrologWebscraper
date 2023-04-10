:- use_module(library(sgml)).
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).


url(Dept, CourseNum,URL) :-
    atomic_list_concat(['https://courses-test.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-course&dept=',Dept,'&course=',CourseNum],
         URL).
    

download_page(URL, HTML) :-
    http_open(URL, In, []),
    load_html(In, HTML, []),
    close(In).




% extract_data(Html, Sections) :-
%     xpath(Html, //tr(contains(@class,'section')), Table),
%     xpath(Table, //td(text), Sections).

extract_data(Html, CourseName, Sections) :-
    xpath(Html, //table(contains(@class,'section-summary')), Table),
    findall(component(CourseName, Type, Term, [[duration(Start, End, Day, SectionName)]]), (
        extract_section(Table, Type, Start,End,Day,SectionName, Term)
    ), Sections).


grab_after_delimiter(String, Delimiter, Substring) :-
    sub_string(String, AfterDelimiter, _, 0, Delimiter),
    sub_string(String, AfterDelimiter, _, 0, Substring).

extract_section(Table, Type, Start, End, Day, SectionName, Term) :-
    xpath(Table, //tr(contains(@class,'section')), Section),
    % writeln(Section),
    xpath(Section, //td(2,text(string)), SectionName),
    % writeln(SectionName),
    xpath(Section, //td(3,text(string)), Type),
    % writeln(Type),
    xpath(Section, //td(4,number), Term),
    % writeln(Term),
    % xpath(Section, //td(5,text), Delivery),
    % writeln(Delivery),
    xpath(Section, //td(7,text(string)), Days), % need to parse this
    % writeln(Days),
    re_matchsub("Mon|Tue|Wed|Thu|Fri", Days, ReDay),
    dict_pairs(ReDay, _, Pairs),
    memberchk(_-Day, Pairs),
    % writeln(Day),
    xpath(Section, //td(8,text(string)), Start),
    % writeln(Start),
    xpath(Section, //td(9,text(string)), End).
    % writeln(End).


    
%  unpack data

% [CPSC 100 101])]), -> regex using course name and number
% element(td,[],[Lecture]), -> regex using lecture, laboratory, tutorial
% element(td,[],[1]), -> regex using 1 or 2
% element(td,[],[In-Person                     ]), -> regex using In-Person, Online, Hybrid
% element(td,[],[]),
% element(td,[],[ Mon Wed Fri]),
% element(td,[],[15:00]),
% element(td,[],[16:00]),


save_html_to_file(Data) :-
    open('output.txt', write, Out),
    write(Out, Data),
    close(Out).

run_web_scraper :-

    % user input
    writeln("Please input the courses you wish to enrol in, separated by commas (ex. ""CPSC 100, FNH 150""):"),
    read_line_to_string(user_input, Input),
    split_string(Input, ",", "", DeptCourseNums),
    maplist(web_scraper_helper, DeptCourseNums).

web_scraper_helper(CourseNumber) :-
    split_string(DeptCourseNum, " ", "", [DeptStr, CourseNumStr]),
    atom_chars(CourseNumber, CourseNumStr),
    atom_chars(Dept, DeptStr),
    url(Dept, CourseNumber, URL),
    writeln(Data),
    save_html_to_file(Data),
    sleep(5).



%  figure out this shit


% component("CS312", "Lecture", [[duration(clock_time(10,00), clock_time(11,20, T)), duration(clock_time(10,00), clock_time(11,20, Th))],
%                               [duration(clock_time(12,00), clock_time(1,20, T)), duration(clock_time(12,00), clock_time(1,20, Th))]])
% component(Name, Type, [[duration(Start, End, Day)]]).

