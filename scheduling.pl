:- module(scheduling).
:- include(scheduleutils).

% Component -> Component 
% create component with sorted sections
% returns the component with sorted sections
sort_sections(component(Course, Type, Sections), component(Course, Type, SortedSections)) :-
    predsort(compare_ending_time, Sections, SortedSections).

% Order -> Section -> Section
% comparison predicate to compare two sections based on ending time
% returns the Order to make earlier ending time first in a list
compare_ending_time(Order, section(_, _, clock_time(Hr1, Min1), DayAtom1, _), section(_, _, clock_time(Hr2, Min2), DayAtom2, _)) :-
    compare_day(OrderDay, DayAtom1, DayAtom2),
    (OrderDay = (=) ->
        compare_hour(OrderHour, Hr1, Hr2),
        (OrderHour = (=) ->
            compare_min(Order, Min1, Min2)
        ;
            Order = OrderHour
        )
    ;
        Order = OrderDay
    ).

% Order -> Day -> Day
%  comparison predicate to compare two days based on day name
% returns the Order to make earlier day first in a list
compare_day(Order, Day1, Day2) :-
    day_to_number(Day1, Num1),
    day_to_number(Day2, Num2),
    compare(Order, Num1, Num2).

% Order -> Hour -> Hour
%  comparison predicate to compare two hours
% returns the Order to make earlier hour first in a list
compare_hour(Order, Hr1, Hr2) :-
    compare(Order, Hr1, Hr2).

% Order -> Minute -> Minute 
%  comparison predicate to compare two minutes
% returns the Order to make earlier minute first in a list
compare_min(Order, Min1, Min2) :-
    compare(Order, Min1, Min2).

% Day -> Number
% Function to convert day name to number for comparison
% returns the number of the day
day_to_number("Sun", 0).
day_to_number("Mon", 1).
day_to_number("Tue", 2).
day_to_number("Wed", 3).
day_to_number("Thu", 4).
day_to_number("Fri", 5).
day_to_number("Sat", 6).

% ComponentList -> ComponentList
% sort components based on number of sections
% returns the sorted component list
sort_components(ComponentList, SortedComponentList) :-
    predsort(compare_sections, ComponentList, SortedComponentList).

% Order -> Component -> Component
% Comparison predicate to compare two components based on number of sections
% returns the Order to make less sections first in a list
compare_sections(Order, component(_,_,Sections1), component(_,_,Sections2)) :-
    length(Sections1, Length1),
    length(Sections2, Length2),
    compare(Order, Length1, Length2).

% ComponentList -> ComponentList -> Schedule -> Schedule
% add a section to schedule if it does not overlap with any other section in schedule; remove component as it is scheduled
% returns the new component list and new schedule
add_schedule([component(Name, Type, [Section|Sections])|Rest], Rest, Schedule, [Section|Schedule]) :-
    no_overlap(Section, Schedule).


% ComponentList -> ComponentList -> Schedule -> Schedule
% remove section from component if it overlaps with any other section in schedule; leave schedule the same
% returns the new component and same schedule
add_schedule([component(Name, Type, [Section|Sections])|Rest], [component(Name, Type, Sections)|Rest], Schedule, Schedule) :-
    not(no_overlap(Section, Schedule)).


% ComponentList -> Schedule -> Schedule
% produce the schedule as result if no components are left
do_schedule([], Schedule, Schedule).


% ComponentList -> Schedule -> Schedule
% schedule the components given a list of comopnents, an accumulator schedule and a new schedule
% returns the new schedule
do_schedule(Components, Schedule, NewSchedule) :-
    add_schedule(Components, ResComps, Schedule, ResSched),
    sort_components(ResComps, SortedComps),
    do_schedule(SortedComps, ResSched, NewSchedule).

% StringList -> ComponentList -> Schedule
% sorts the components, sorts the sections in each component, and schedules the components
% returns the schedule
start_scheduling(Classes, Data, Schedule) :-
    sort_components(Data, SortedCompLen),
    maplist(sort_sections, SortedCompLen, SortedComp),
    do_schedule(SortedComp, [], Schedule).
