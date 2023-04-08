:- module(scheduling).

/*
% we read info from scraper here
read_output_file(Lines) :-
    open("output.txt", read, Stream),
    read_lines(Stream, Lines),
    close(Stream).

read_lines(Stream, []) :-
    at_end_of_stream(Stream).

read_lines(Stream, [Line|Rest]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream, Codes),
    atom_codes(Line, Codes),
    read_lines(Stream, Rest).

% setting up the scheduling algorithm
*/



% the main function to make a schedule.
% the function takes a list of class names, and creates a schedule for the student including all of those classes.
start_scheduling(Classes, Data, S) :-
    make_schedule(map_find_components(Classes, Data), S).

% converts the names of the courses into their comopnents by searching components in the passed data.
map_find_components([], L, L).
map_find_components([H|T], L, append(H1, L1)) :-
    H1 is find_components(H, Data),
    map_find_components(T, L, L1).

% searches for a class with the given name, and returns a list of its components.
find_components(Name, [class(Name, Components)|_], Components).
find_components(Name, [_|R]) :- find_components(Name, R).

% creates a schedule for the user using the list of comopnents provided. with each component, it will check if a valid schedule can be made using the
% first set of durations. if so, that schedule will be used. if not, it will search through the remaining durations.

% if the list of components is empty, return the existing schedule.
make_schedule([], S, S).
% if a valid schedule can be made with the current durations, make the registration,
% and append it to the schedule returned by recursion.
make_schedule([component(Name, Type, [D|_])|R], S, [R|S1]) :-
    R is registration(Name, Type, D),
    remove_overlaps_list(D, R, []),
    make_schedule(R, S, S1).
% if no valid schedule can be made, try the next durations
make_schedule([component(Name, Type, [_|D1])|R], S, S1) :-
    make_schedule([component(Name, Type, D1)|R], S, S1).
% if a component is out of durations, do not register it.
make_schedule([component(Name, Type, [])|R], S, S1) :-
    make_schedule(R, S, S1).

% given a list of components, remove every duration for each component that overlaps with the given registration.
remove_overlaps_list([], L, L).
remove_overlaps_list(DL, [component(Name, Type, DL1)|R], L, [component(Name, Type, D1)|L1]) :-
    D1 is remove_overlaps(DL, DL1),
    remove_overlaps_list(DL, R, L, L1).


