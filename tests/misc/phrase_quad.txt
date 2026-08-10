c1 ?- call(1).
      type_error(callable,1).

c2 ?- call((1,fail)).
      type_error(callable,(1,fail)).

c3 ?- call((fail,1)).
      type_error(callable,(fail,1)).

c4 ?- call((!;1)).
      type_error(callable,(!;1)).

c5 ?- call((!;\+1)).
      true.

c6 ?- call((!;call(1))).
      true.

c7 ?- call((\+!;X=a)).
      X=a.

f1 ?- functor(T,F,A).
      instantiation_error.

f2 ?- functor(T,1,2).
      type_error(atom,1).

f3 ?- functor([_],.,2).
      true.

1  ?- phrase(=,L).
      L = []
   |  existence_error(non_terminal,(=)//0).

2  ?- phrase(1,L).
      type_error(callable,1).

39 ?- phrase(K,L).
      instantiation_error.

40 ?- K = [], phrase(K,L).
      K = [], L = [].

24 ?- asserta((a-->b)).
      permission_error(modify,static_procedure,(-->)/2).

25 ?- clause((a-->b),B).
      permission_error(access,private_procedure,(-->)/2).

26 ?- (X-->Y).
      existence_error(procedure,(-->)/2).

3  ?- phrase(!,L).
      L = [].

4  ?- phrase([a],L).
      L = [a].

5  ?- phrase([a|b],L).
      type_error(list,[a|b]).

6  ?- phrase([a|L],K).
      instantiation_error.

38 ?- phrase([a|L],L).
      instantiation_error.

7  ?- phrase([a|L],[a,b]).
      instantiation_error.

8  ?- phrase([a|L],[]).
      instantiation_error.

9  ?- phrase(([a],[]),[a]).
      true.

10 ?- phrase(([a],{1}),[]).
      type_error(callable,(...,...)).

37 ?- phrase((!,[a],{1}),[]).
      type_error(callable,(...,...)).

11 ?- phrase(({!,fail};[]),L).
      false.

12 ?- phrase('|'([],[a]),[a]).
      true.

13 ?- phrase(({fail},1),L).
      type_error(callable,1).

14 ?- phrase(([a];[]),L).
      L = [a] ; L = [].

15 ?- phrase({fail,1},L).
      type_error(callable,((fail,1),[]=_A))
   |  type_error(callable,((fail,1),...)).

16 ?- phrase({throw(h)},[a]).
      throw(h).

17 ?- phrase(({L=[]},[a|L]),[a]).
      instantiation_error.

18 ?- phrase([a|L],K), L=[b].
      instantiation_error.

19 ?- phrase(([a|L],1),[]).
      type_error(callable,1)
   |  instantiation_error.

20 ?- phrase((1,[a|L]),[]).
      type_error(callable,1)
   |  instantiation_error.

21 ?- phrase((1,[a|b]),[]).
      type_error(callable,1)
   |  type_error(list,[a|b]).

46 ?- phrase((1,{2}),[]).
      type_error(callable,1).

47 ?- phrase(({2},1),[]).
      type_error(callable,1).

48 ?- phrase((1,(2,[_|_],3),4),[]).
      type_error(callable,1)
   |  type_error(callable,2)
   |  instantiation_error
   |  type_error(callable,3)
   |  type_error(callable,4).


22 ?- phrase('|'(([x]->[y]),[z]),L).
      representation_error(dcg_body)
   |  L=[x,y].

23 ?- phrase(;(([x]->[y]),[z]),L).
      L=[x,y].

45 ?- phrase(([a],phrase(2)),[]).
      false.

27 ?- phrase(\+[a],[]).
      representation_error(dcg_body)
   |  true.

28 ?- phrase(\+1,L).
      representation_error(dcg_body)
   |  type_error(callable,1).

29 ?- phrase(([a],\+1),[]).
      representation_error(dcg_body)
   |  false.

30 ?- phrase(([a],\+1;[]),[]).
      representation_error(dcg_body)
   |  true.

31 ?- phrase(phrase(phrase,[]),L).
      existence_error(procedure,phrase/4)
   |  existence_error(non_terminal,phrase//2)
   |  L = [].

32 ?- phrase(call([]),[]).
      existence_error(procedure,[]/2)
   |  true.

33 ?- L=[],phrase([a|L],[b]).
      false.

36 ?- L=[],phrase([a|L],[a]).
      L=[].

34 ?- phrase([a],[b]).
      false.

35 ?- phrase(!,[_]) ; L=1.
      L=1.

41 ?- phrase([], non_list).
      false
   |  type_error(list,non_list).

42 ?- phrase([], [a|non_list]).
      false
   |  type_error(list,[a|non_list]).

43 ?- phrase([], L,non_list).
      L = non_list
   |  type_error(list,non_list).

44 ?- phrase([], L,[a|non_list]).
      L = [a|non_list]
   |  type_error(list,[a|non_list]).
