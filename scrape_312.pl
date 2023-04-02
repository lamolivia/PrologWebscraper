:- use_module(library(sgml)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).

url("https://www.cs.ubc.ca/~poole/cs312/2023/schedule.html").
data_to_extract([title, description, keywords]).

download_page(URL, HTML) :-
    http_open(URL, In, []),
    load_html(In, HTML, []),
    close(In).

extract_data(HTML, Data) :-
    findall(Value, (
        member(TagName, Data),
        xpath(HTML, //TagName, TextNode),
        atom_string(Value, TextNode)
    ), DataValues),
    maplist(atom_string, Data, DataStrings),
    maplist(atom_string, DataValues, DataValueStrings),
    Data =.. [DataStrings, DataValueStrings].

store_data(_) :-
    open('data.csv', append, File),
    maplist(write_csv_line(File), Data),
    close(File).

write_csv_line(File, Data) :-
    csv_write_stream(File, [Data], []).

run_web_scraper :-
    url(URL),
    download_page(URL, HTML),
    data_to_extract(Data),
    extract_data(HTML, DataValues),
    store_data(DataValues).
