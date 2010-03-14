% -*- prolog -*-

% siteswap.pl
%
% Copyright (C) 2006-2010 Mark Probst
%
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU General Public License
% as published by the Free Software Foundation; either version 2
% of the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

% To generate a puzzle, call the predicate
% quasi_random_puzzle_and_solution/2, like this:
%
%   quasi_random_puzzle_and_solution(P, S).
%
% Sometimes it'll fail, sometimes, you'll get solutions.  P will be
% bound to a puzzle with unbound variables for the "unknowns".  S will
% be bound to the (only) solution.

is_siteswap([T0,T1,T2]) :-
	fd_domain([M0,M1,M2],0,2),
	M0 #= (T0 rem 3),
	M1 #= ((T1 + 1) rem 3),
	M2 #= ((T2 + 2) rem 3),
	fd_all_different([M0,M1,M2]).
is_siteswap([T0,T1,T2,T3]) :-
	fd_domain([M0,M1,M2,M3],0,3),
	M0 #= (T0 rem 4),
	M1 #= ((T1 + 1) rem 4),
	M2 #= ((T2 + 2) rem 4),
	M3 #= ((T3 + 3) rem 4),
	fd_all_different([M0,M1,M2,M3]).
is_siteswap([T0,T1,T2,T3,T4]) :-
	fd_domain([M0,M1,M2,M3,M4],0,4),
	M0 #= (T0 rem 5),
	M1 #= ((T1 + 1) rem 5),
	M2 #= ((T2 + 2) rem 5),
	M3 #= ((T3 + 3) rem 5),
	M4 #= ((T4 + 4) rem 5),
	fd_all_different([M0,M1,M2,M3,M4]).

is_nontrivial_siteswap(S) :-
	S = [T0,T1,T2],
	is_siteswap(S),
	(T0 #\= T1) #\/ (T0 #\= T2) #\/ (T1 #\= T2).
is_nontrivial_siteswap(S) :-
	S = [T0,T1,T2,T3],
	is_siteswap(S),
	(T0 #\= T1) #\/ (T0 #\= T2) #\/ (T0 #\= T3) #\/ (T1 #\= T2) #\/ (T1 #\= T3) #\/ (T2 #\= T3).
is_nontrivial_siteswap(S) :-
	S = [T0,T1,T2,T3,T4],
	is_siteswap(S),
	(T0 #\= T1) #\/ (T0 #\= T2) #\/ (T0 #\= T3) #\/ (T0 #\= T4) #\/
    	(T1 #\= T2) #\/ (T1 #\= T3) #\/ (T1 #\= T4) #\/
    	(T2 #\= T3) #\/ (T2 #\= T4) #\/
	(T3 #\= T4).

siteswaps_different([S0,S1,S2],[T0,T1,T2]) :-
	((S0 #\= T0) #\/ (S1 #\= T1) #\/ (S2 #\= T2)) #/\
	((S0 #\= T1) #\/ (S1 #\= T2) #\/ (S2 #\= T0)) #/\
	((S0 #\= T2) #\/ (S1 #\= T0) #\/ (S2 #\= T1)).
siteswaps_different([S0,S1,S2,S3],[T0,T1,T2,T3]) :-
	((S0 #\= T0) #\/ (S1 #\= T1) #\/ (S2 #\= T2) #\/ (S3 #\= T3)) #/\
	((S0 #\= T1) #\/ (S1 #\= T2) #\/ (S2 #\= T3) #\/ (S3 #\= T0)) #/\
	((S0 #\= T2) #\/ (S1 #\= T3) #\/ (S2 #\= T0) #\/ (S3 #\= T1)) #/\
	((S0 #\= T3) #\/ (S1 #\= T0) #\/ (S2 #\= T1) #\/ (S3 #\= T2)).
siteswaps_different([S0,S1,S2,S3,S4],[T0,T1,T2,T3,T4]) :-
	((S0 #\= T0) #\/ (S1 #\= T1) #\/ (S2 #\= T2) #\/ (S3 #\= T3) #\/ (S4 #\= T4)) #/\
	((S0 #\= T1) #\/ (S1 #\= T2) #\/ (S2 #\= T3) #\/ (S3 #\= T4) #\/ (S4 #\= T0)) #/\
	((S0 #\= T2) #\/ (S1 #\= T3) #\/ (S2 #\= T4) #\/ (S3 #\= T0) #\/ (S4 #\= T1)) #/\
	((S0 #\= T3) #\/ (S1 #\= T4) #\/ (S2 #\= T0) #\/ (S3 #\= T1) #\/ (S4 #\= T2)) #/\
	((S0 #\= T4) #\/ (S1 #\= T0) #\/ (S2 #\= T1) #\/ (S3 #\= T2) #\/ (S4 #\= T3)).

are_siteswaps([]).
are_siteswaps([S | Ss]) :-
	is_nontrivial_siteswap(S),
	are_siteswaps(Ss).

siteswap_different_from_siteswaps(_, []).
siteswap_different_from_siteswaps(S, [T | Ts]) :-
	siteswaps_different(S, T),
	siteswap_different_from_siteswaps(S, Ts).

all_siteswaps_different([]).
all_siteswaps_different([S | Ss]) :-
	siteswap_different_from_siteswaps(S, Ss),
	all_siteswaps_different(Ss).

puzzle_constraints_3x4([N00,N01,N02,N03,N10,N11,N12,N13,N20,N21,N22,N23]) :-
	R0 = [N00,N01,N02,N03],
	R1 = [N10,N11,N12,N13],
	R2 = [N20,N21,N22,N23],
	C0 = [N00,N10,N20],
	C1 = [N01,N11,N21],
	C2 = [N02,N12,N22],
	C3 = [N03,N13,N23],
	are_siteswaps([R0,R1,R2,C0,C1,C2,C3]),
	all_siteswaps_different([R0,R1,R2]),
	all_siteswaps_different([C0,C1,C2,C3]).

puzzle_constraints_4x5([N00,N01,N02,N03,N04,N10,N11,N12,N13,N14,N20,N21,N22,N23,N24,N30,N31,N32,N33,N34]) :-
	R0 = [N00,N01,N02,N03,N04],
	R1 = [N10,N11,N12,N13,N14],
	R2 = [N20,N21,N22,N23,N24],
	R3 = [N30,N31,N32,N33,N34],
	C0 = [N00,N10,N20,N30],
	C1 = [N01,N11,N21,N31],
	C2 = [N02,N12,N22,N32],
	C3 = [N03,N13,N23,N33],
	C4 = [N04,N14,N24,N34],
	are_siteswaps([R0,R1,R2,R3,C0,C1,C2,C3,C4]),
	all_siteswaps_different([R0,R1,R2,R3]),
	all_siteswaps_different([C0,C1,C2,C3,C4]).

puzzle_3x4(L) :-
	length(L, 12),
	fd_domain(L, 1, 9),
	puzzle_constraints_3x4(L),
	fd_labeling(L).

puzzle_4x5(L) :-
	length(L, 20),
	fd_domain(L, 1, 9),
	puzzle_constraints_4x5(L),
	fd_labeling(L).

quasi_random_puzzle_3x4(L) :-
	L = [N00,N01,_,_,_,N11,_,_,_,_,N22,_],
	random(1, 9, N00), random(1, 9, N01), random(1, 9, N11), random(1, 9, N22),
	puzzle_3x4(L).

quasi_random_puzzle_4x5(L) :-
	L = [N00,N01,_,  _,  _,
	     _  ,N11,_,  _,  _,
	     _,  _,  N22,_,  _,
	     _,  _,  _,  N33,_],
	random(1, 9, N00), random(1, 9, N01), random(1, 9, N11), random(1, 9, N22), random(1, 9, N33),
	puzzle_4x5(L).

puzzle_solutions(Puzzle, Solutions, Solver) :-
	findall(Puzzle, call(Solver, Puzzle), Solutions).

replace_nth([_|Rest], 1, Elem, [Elem|Rest]).
replace_nth([L|Ls], Pos, Elem, [L|New_Ls]) :-
	Pos > 1,
	Pos_minus_1 is Pos - 1,
	replace_nth(Ls, Pos_minus_1, Elem, New_Ls).

depopulate_puzzle(Puzzle, Empty_puzzle, Solver) :-
	length(Puzzle, Length),
	random(1, Length, Pos),
	replace_nth(Puzzle, Pos, _, New_puzzle),
	next_depopulate_step_or_succeed(Puzzle, New_puzzle, Empty_puzzle, Solver).

next_depopulate_step_or_succeed(Puzzle, New_puzzle, Empty_puzzle, Solver) :-
	puzzle_solutions(New_puzzle, [_, _ | _], Solver),
	puzzle_solutions(Puzzle, [_], Solver),
	Empty_puzzle = Puzzle.
next_depopulate_step_or_succeed(_, New_puzzle, Empty_puzzle, Solver) :-
	puzzle_solutions(New_puzzle, [_], Solver),
	depopulate_puzzle(New_puzzle, Empty_puzzle, Solver).

quasi_random_puzzle_3x4_and_solution(Puzzle, Solution) :-
	quasi_random_puzzle_3x4(Solution),
	depopulate_puzzle(Solution, Puzzle, puzzle_3x4).

quasi_random_puzzle_4x5_and_solution(Puzzle, Solution) :-
	quasi_random_puzzle_4x5(Solution),
	depopulate_puzzle(Solution, Puzzle, puzzle_4x5).
