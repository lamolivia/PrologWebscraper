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





save_html_to_file(Data) :-
    open('output.txt', write, Out),
    write(Out, Data),
    close(Out).

run_web_scraper :-

    %  I assume you will want to pass in the course name and number for user input
    url('CPSC', '100', URL),

    download_page(URL, HTML),
    atom_concat('CPSC','100', CourseName),
    %  we want to extract all data into tuples
    extract_data(HTML, CourseName, Data),
    % writeln(Data),

    save_html_to_file(Data).


