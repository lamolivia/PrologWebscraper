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
% predicate that reflects a 24-hour time, in hours and minutes
time(Hour, Minute).

% predicate reflecting the duration of a component of a class. it has the time it starts and the time it ends, and the day of the week it occurs on
duration(Start, End, Day).
% a component of a class, such as a lecture or lab. it has the name of the course, the component type,
% and a list of possible durations.
% example: a lecture in 312 that can be taken at 10 AM or noon on tuesdays and thursdays would be the following format:
% component("CS312", "Lecture", [[duration(time(10,00), time(11,20, T)), duration(time(10,00), time(11,20, Th))],
%                               [duration(time(12,00), time(1,20, T)), duration(time(12,00), time(1,20, Th))]])
component(Name, Type, [[duration(Start, End, Day)]]).

% a class with a list of all the required components. 
class(Name, [Components]).

% a registration in a class. it has the name of the class and the type of activity that has been registered, as well as
% the durations of that activity.
registration(Name, Type, [duration(Start, End, Day)]).

% the main function to make a schedule.
% the function takes a list of class names, and creates a schedule for the student including all of those classes.
make_schedule([], Data, S).
make_schedule([Name|Rest], Data, S, S2) :-
    make_schedule(Rest, Data, S1, S2),
    schedule_class(find_class(Name, Data),S,S1).

% searches out a class in the data by name.
find_class(Name, [class(Name, [Components])|_], class(Name, [Components])).
find_class(Name, [_|R]) :- find_class(Name, R).

% a program to register the user in all the components of a single class.
% the function takes a class and the existing schedule, and returns a new schedule with all components added.

% if a class has no components, the schedule is uneffected 
schedule_class(class(Name, []), S, S).
% if there are components, first call make_registration to register that component into the schedule at a valid time.
% then, call itself recursively on the remaining components in the list.
schedule_class(class(_, [H|T]), S, S2) :-
    schedule_class(class(_, T), S1),
    make_registration(H, S, S1).

% takes a component with a list of its durations, and an existing schedule.
% every component will have several possible durations, represented in the provided list. 
% each item of the durations list is checked against the existing registrations to find a list with no overlap.

%
make_registration(component(Name, Type, [H|T], [], [R])) :-
    R = registration(Name, Type, H).
make_registration(component(Name, Type, [H|T]), S, [R|S]) :-
    no_overlaps(H, S), 
    R = registration(Name, Type, H).
make_registration(component(Name, Type, [H|T]), S, S1):-
    make_registration(component(Name, Type, T), S, S1).

no_overlaps(_, []) :- true.
no_overlaps([], _) :- true.
no_overlaps([H|T], S) :- 
    no_dur_overlap(H, S),
    no_overlaps(T, S).

no_dur_overlap(_, []) :- true.
no_dur_overlap(D, [registration(_, _, DL)|R]) :-
    check_overlaps_list(D, DL),
    no_dur_overlap(D, R).

check_overlaps_list(D, [H|T]) :-
    check_overlap(D, H),
    check_overlaps_list(D, T).

check_overlap(duration(S, E, D), check_overlap(S1, E1, D1)) :-
    before(E1, S); before(E, S1); D \= D1.

before(time(H, M), time(H1, M1)) :-
    H < H1 ; H = H1, M < M1.