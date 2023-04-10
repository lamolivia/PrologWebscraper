:- module(scheduleutils).

% predicate that reflects a 24-hour clock_time, in hours and minutes
clock_time(Hour, Minute).

% predicate reflecting the section of a component of a class. it has the clock_time it starts and the clock_time it ends, and the day of the week it occurs on
section(SecName, Start, End, Day, Term).
% a component of a class, such as a lecture or lab. it has the name of the course, the component type,
% and a list of possible sections.
% example: a lecture in 312 that can be taken at 10 AM or noon on tuesdays and thursdays would be the following format:
% component("CS312", "Lecture", [[section(clock_time(10,00), clock_time(11,20), T), section(clock_time(10,00), clock_time(11,20), Th)],
%                               [section(clock_time(12,00), clock_time(1,20), T), section(clock_time(12,00), clock_time(1,20), Th)]])
component(Name, Type, [[section(SecName, Start, End, Day, Term)]]).

% a class with a list of all the required components. 
class(Name, [Components]).

% a registration in a class. it has the name of the class and the type of activity that has been registered, as well as
% the sections of that activity.
registration(Name, Type, [section(SecName, Start, End, Day, Term)]).

% every registration has a list of sections, so to remove all


% each registration has a list of associated secs, so when this is called to remove all incompatible section sublists from other components,
% we need this recursive solution
remove_overlaps_secs(_, [], []).
remove_overlaps_secs([], L, L).
remove_overlaps_secs([S1|T], L, L2) :-
    remove_overlaps_secs(T, L1, L2),
    remove_overlaps(S1, L, L1).

% takes a section, Sec and list of lists of sections. removes every item from the list where any of their sections overlap Sec.
remove_overlaps(Sec, [], []).
remove_overlaps(Sec, [H|T], [H|L1]) :-
    no_overlap(Sec, H),
    remove_overlaps(Sec, T, L1).
remove_overlaps(Sec, [H|T], L1) :- 
    remove_overlaps(Sec, T, L1).

% returns true only if no section in the list overlaps the section passed.
no_overlap(_, []) :- true.
% return true if two occur in different terms
no_overlap(section(SN, S, E, D, T), [section(SN1, S1, E1, D1, T1)|Rest]) :-
    T \== T1,
    no_overlap(section(SN, S, E, D, T), Rest).
% return true if two occur on different days
no_overlap(section(SN, S, E, D, T), [section(SN1, S1, E1, D1, T1)|Rest]) :-
    D \== D1,
    no_overlap(section(SN, S, E, D, T), Rest).
% return true if one ends before the other starts
no_overlap(section(SN, S, E, D, T), [section(SN1, S1, E1, D1, T1)|Rest]) :-
    before(E1, S),
    no_overlap(section(SN, S, E, D, T), Rest).
% ditto
no_overlap(section(SN, S, E, D, T), [section(SN1, S1, E1, D1, T1)|Rest]) :-
    before(E, S1),
    no_overlap(section(SN, S, E, D, T), Rest).
% return false if all other rules fail
no_overlap(_,_) :- false.

% returns true if one clock_time happens before another.
before(clock_time(H, M), clock_time(H1, M1)) :-
    H < H1 ; H = H1, M < M1.