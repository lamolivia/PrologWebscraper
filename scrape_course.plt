:- begin_tests(scrape_course).
:- include(scrape_course).

test_html(Data) :- Data = '<html><body><table class="section-summary"><tr><td>My</td></tr><tr><td>Data</td></tr></table></body></html>'.
test_url(URL) :- URL = "https:oliviastestingurl.com".

test(extract_data) :-
    Expected = ['My', 'Data'],
    test_html(HTML),
    extract_data(HTML, Data),
    assertion(Data == Expected).

test(run_web_scraper) :-
    Expected = ['My', 'Data'],
    test_url(URL),
    test_html(HTML),
    download_page(URL, HTML),
    extract_data(HTML, Data),
    assertion(Data == ExpectedData),
    save_html_to_file(Data),
    assertion(read_file_to_string('output.txt', FileContent, []) == "My\nData\n").


:- end_tests(scrape_course).