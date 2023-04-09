:- use_module(library(sgml)).
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).

url(Dept, CourseNumber, URL) :-
    format(atom(URL), 'https://courses-test.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-course&dept=~w&course=~w', [Dept, CourseNumber]).

download_page(URL, HTML) :-
    http_open(URL, In, []),
    load_html(In, HTML, []),
    close(In).


extract_data(Html, Data) :-
    xpath(Html, //tr(contains(@class,'section')), Table),
    xpath(Table, //td(text), Data).


save_html_to_file(Data) :-
    open('output.txt', write, Out),
    write(Out, Data),
    close(Out).

run_web_scraper :-
    % user input
    writeln("Please input the courses you wish to enrol in, separated by commas (ex. ""CPSC 100, FNH 150""):"),
    read_line_to_string(user_input, Input),
    split_string(Input, ",", "", DeptCourseNums),
    maplist(web_scraper_helper, DeptCourseNums)

web_scraper_helper(CourseNumber) :-
    split_string(DeptCourseNum, " ", "", [DeptStr, CourseNumStr]),
    atom_chars(CourseNumber, CourseNumStr),
    atom_chars(Dept, DeptStr),
    url(Dept, CourseNumber, URL),
    download_page(URL, HTML),
    extract_data(HTML, Data),
    %  we want to extract all data into tuples
    writeln(Data),

    save_html_to_file(Data).

