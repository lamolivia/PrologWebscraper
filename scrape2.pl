:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(xpath)).
:- use_module(library(http/html_write)).

% URL to scrape
url("https://courses-test.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-course&dept=CPSC&course=100").

% Download a web page as a string
:- use_module(library(process)).

download_page(URL, Page) :-
    process_create(path(curl), ['-L', '-s', '--globoff', '--remote-header-name', URL], [stdout(pipe(Out))]),
    read_string(Out, _, Page).

% Extract table rows from HTML
extract_rows(HTML, Rows) :-
    xpath(HTML, //tr(normalize_space), Rows).

% Scrape the web page and print the results
run_web_scraper :-
    url(URL),
    download_page(URL, Page),
    load_html(Page, HTML, []),
    extract_rows(HTML, Rows),
    print_html(Rows).
