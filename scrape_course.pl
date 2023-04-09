:- use_module(library(sgml)).
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).

url(CourseNumber, URL) :-
    format(atom(URL), 'https://courses-test.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-course&dept=CPSC&course=~w', [CourseNumber]).

download_page(URL, HTML) :-
    http_open(URL, In, []),
    load_html(In, HTML, []),
    close(In).


extract_data(Html, Data) :-
    xpath(Html, //table(contains(@class,'section-summary')), Table),
    xpath(Table, //td(text), Data).


save_html_to_file(Data) :-
    open('output.txt', write, Out),
    write(Out, Data),
    close(Out).

run_web_scraper(CourseNumber) :-
    url(CourseNumber, URL),
    download_page(URL, HTML),
    extract_data(HTML, Data),
    %  we want to extract all data into tuples
    writeln(Data),

    save_html_to_file(Data).

