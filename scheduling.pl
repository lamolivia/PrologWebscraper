:- module(scheduling).
:- include(scheduleutils).










% Function to sort sections in a component by ending clock time
sort_sections([component, Course, Type, Sections], [component, Course, Type, SortedSections]) :-
    predsort(compare_ending_time, Sections, SortedSections).

% Comparison predicate to compare two sections based on their ending clock time and day
compare_ending_time(Order, section(_, _, EndTime1, DayAtom1, _), section(_, _, EndTime2, DayAtom2, _)) :-
    day_to_number(DayAtom1, Day1),
    day_to_number(DayAtom2, Day2),
    compare(OrderDay, Day1, Day2),
    (OrderDay == 0 ->
        compare(OrderTime, EndTime1, EndTime2),
        Order = OrderTime;
        Order = OrderDay
    ).

% Function to convert day name to number for comparison
day_to_number(Mon, 1).
day_to_number(Tue, 2).
day_to_number(Wed, 3).
day_to_number(Thu, 4).
day_to_number(Fri, 5).
day_to_number(Sat, 6).
day_to_number(Sun, 7).


% Function to sort components based on number of sections
sort_components(ComponentList, SortedComponentList) :-
    predsort(compare_sections, ComponentList, SortedComponentList).

% Comparison predicate to compare two components based on number of sections
compare_sections(Order, component(_,_,Sections1), component(_,_,Sections2)) :-
    length(Sections1, Length1),
    length(Sections2, Length2),
    compare(Order, Length1, Length2).
%  try break ties on earliest if possible but dont bother if impossible


add_schedule([component(Name, Type, [Section|Sections])|Rest], Rest, Schedule, [Section|Schedule]) :-
    no_overlap(Section, Schedule).

add_schedule([component(Name, Type, [Section|Sections])|Rest], [component(Name, Type, Sections)|Rest], Schedule, Schedule) :-
    not(no_overlap(Section, Schedule)).

do_schedule([], Schedule, Schedule).
do_schedule(Components, Schedule, NewSchedule) :-
    add_schedule(Components, ResComps, Schedule, ResSched),
    sort_components(ResComps, SortedComps),
    do_schedule(SortedComps, ResSched, NewSchedule).


start_scheduling(Classes, Data, Schedule) :-
    sort_components(Data, SortedCompLen),
    do_schedule(SortedCompLen, [], Schedule).
