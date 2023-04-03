:- use_module(library(sgml)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).

url("https://www.cs.ubc.ca/~poole/cs312/2023/schedule.html").

% there is def a re_match("Mar 1")

download_page(URL, HTML) :-
    http_open(URL, In, []),
    load_html(In, HTML, []),
    close(In).

parse_dates(Text, Month, Day) :-
    re_matchsub("^.*?(Jan|Feb|Mar|April|May|June|July|Aug|Sept|Oct|Nov|Dec)[a-z]*[^0-9]+([0-9]+).*?\\.(.?)", Text, Matches),
    print(Matches),
    dict_pairs(Matches, _, Pairs),
    memberchk(1-Month, Pairs),
    memberchk(2-Day, Pairs),
    %print(Month),
    %print(Day), 
    month_number(Month, _),
    atom_number(Day, DayNum).
    day_number(DayNum). 

% could put an if for feb if want
day_number(D) :-
    between(0, 31, D).

month_number("Jan", 1).
month_number("January", 1).
month_number("Feb", 2).
month_number("February", 2).
month_number("Mar", 3).
month_number("March", 3).
month_number("Apr", 4).
month_number("April", 4).

% these dates fall after term ends
month_number("May", 5).
month_number("June", 6).
month_number("July", 7).
month_number("Aug", 8).
month_number("Sept", 9).
month_number("Oct", 10).
month_number("Nov", 11).
month_number("Dec", 12).

extract_data(HTML, Data) :-
    findall(Date, (
        xpath(HTML, //(ul), UL),
        xpath(UL, //li(text), TextNode),
        atom_string(Value, TextNode),
        %print(Value),
        parse_dates(Value, MonthNumber, Day),
        %print(MonthNumber),
        format(atom(Date), '~w ~w', [MonthNumber, Day])
    ), DataValues),
    maplist(atom_string, DataValues, Data).

run_web_scraper :-
    url(URL),
    download_page(URL, HTML),
    extract_data(HTML, Data),
    writeln(Data).
