:- use_module(library(sgml)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).

url("https://www.cs.ubc.ca/~poole/cs312/2023/schedule.html").

download_page(URL, HTML) :-
    http_open(URL, In, []),
    load_html(In, HTML, []),
    close(In).

extract_data(HTML, Data) :-
    findall(Value, (
        xpath(HTML, //ul, UL),
        xpath(UL, //li(text), TextNode),
        atom_string(Value, TextNode)
    ), DataValues),
    maplist(atom_string, DataValues, Data).

run_web_scraper :-
    url(URL),
    download_page(URL, HTML),
    extract_data(HTML, Data),
    writeln(Data).
