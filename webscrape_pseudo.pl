% other possible resource: https://www.benjaminjohnston.com.au/screenscraping

% Step 1: Define the website you want to scrape
url("https://example.com/").
data_to_extract([title, description, keywords]).

% Step 2: Choose a Prolog web scraping library
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).

% Step 3: Write the code to request and download the web page
download_page(URL, HTML) :-
    http_open(URL, In, []),
    load_html(In, HTML, []),
    close(In).

% Step 4: Parse the HTML content
extract_data(HTML, Data) :-
    findall(Value, (
        member(TagName, Data),
        xpath(HTML, //TagName(text), TextNode),
        atom_string(Value, TextNode)
    ), DataValues),
    maplist(atom_string, Data, DataStrings),
    maplist(atom_string, DataValues, DataValueStrings),
    Data =.. [DataStrings, DataValueStrings].

% Step 5: Store the extracted data
store_data(Data) :-
    open('data.csv', append, File),
    maplist(write_csv_line(File), Data),
    close(File).

write_csv_line(File, Data) :-
    csv_write_stream(File, [Data], []).

% Step 6: Run the web scraper
run_web_scraper :-
    url(URL),
    download_page(URL, HTML),
    data_to_extract(Data),
    extract_data(HTML, DataValues),
    store_data(DataValues).

