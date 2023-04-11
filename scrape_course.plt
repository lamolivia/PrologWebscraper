:- begin_tests(scrape_course).

test(remove_whitespace_char) :-
    remove_whitespace_char(9),
    remove_whitespace_char(10),
    remove_whitespace_char(11),
    remove_whitespace_char(12),
    remove_whitespace_char(13),
    remove_whitespace_char(32),
    \+ remove_whitespace_char(0),
    \+ remove_whitespace_char(foo).

test(remove_whitespace) :-
    remove_whitespace("  C P S C  3 1 2   ", "CPSC312").

test(url) :-
    url("CPSC", "312", URL),
    assertion(URL == "https://courses-test.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-course&dept=CPSC&course=312").

test(extract_data) :-
    load_html_file('test_data.html', HTML, []),
    extract_data(HTML, "CPSC 210", Sections),
    assertion(Sections == [
    component("CPSC 210","Lecture",[section("CPSC 210 101",clock_time(12,30),clock_time(13,30),"Wed",1)])
    ]).

test(extract_section) :-
    load_html_file('test_data.html', HTML, []),
    xpath(HTML, //table(contains(@class,'section-summary')), Table),
    extract_section(Table, Lecture, Start, End, Day, SectionName, Term),
    assertion((
        Start == clock_time(12,30),
        End == clock_time(13,30),
        Day == "Wed",
        SectionName == section("CPSC 210"),
        Term == 1
    )).

:- end_tests(scrape_course).
