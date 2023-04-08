:- module(scheduleutils).

% predicate that reflects a 24-hour clock_time, in hours and minutes
clock_clock_time(Hour, Minute).

% predicate reflecting the duration of a component of a class. it has the clock_time it starts and the clock_time it ends, and the day of the week it occurs on
duration(Start, End, Day).
% a component of a class, such as a lecture or lab. it has the name of the course, the component type,
% and a list of possible durations.
% example: a lecture in 312 that can be taken at 10 AM or noon on tuesdays and thursdays would be the following format:
% component("CS312", "Lecture", [[duration(clock_time(10,00), clock_time(11,20, T)), duration(clock_time(10,00), clock_time(11,20, Th))],
%                               [duration(clock_time(12,00), clock_time(1,20, T)), duration(clock_time(12,00), clock_time(1,20, Th))]])
component(Name, Type, [[duration(Start, End, Day)]]).

% a class with a list of all the required components. 
class(Name, [Components]).

% a registration in a class. it has the name of the class and the type of activity that has been registered, as well as
% the durations of that activity.
registration(Name, Type, [duration(Start, End, Day)]).

% every registration has a list of durations, so to remove all


% each registration has a list of associated durs, so when this is called to remove all incompatible duration sublists from other components,
% we need this recursive solution
remove_overlaps_durs([D1|T], L, L2) :-
    remove_overlaps_durs(T, L1, L2),
    L1 is remove_overlaps(D1, L).

% takes a duration, Dur and list of lists of durations. removes every item from the list where any of their durations overlap Dur.
remove_overlaps(Dur, [], []).
remove_overlaps(Dur, [H|T], [H|L1]) :-
    no_overlap(Dur, H),
    remove_overlaps(Dur, T, L1).
remove_overlaps(Dur, [H|T], L1) :- 
    remove_overlaps(Dur, T, L1).

% returns true only if no duration in the list overlaps the duration passed.
no_overlap(_, []) :- true.
no_overlap(duration(S, E, D), [duration(S1, E1, D1)|T]) :-
    before(E1, S), before(E1, E) ; before(E, S1), before(E, E1); D \= D1,
    no_overlap(duration(S, E, D), T).

% returns true if one clock_time happens before another.
before(clock_time(H, M), clock_time(H1, M1)) :-
    H < H1 ; H = H1, M < M1.