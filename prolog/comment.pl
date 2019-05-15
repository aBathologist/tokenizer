/*
module(tokenize(comment)
       [comment/2,
        comment_rec/2,
        comment_token/3,
        comment_token_rec/3]).
*/
%% Author: Stefan Israelsson Tampe (stefan.itampe@gmail.com)
%% Interface
%% Start and End matchers is either a proper matcher (-->) or an atom to be
%% matched as start or end match of the comment.
%%
%% comment(StartMatcher,EndMatcher)
%%   - anonymously match a non recursive comment
%%
%% comment_rec(StartMatcher,EndMatcher)
%%   - anonymously march a recursive comment
%%
%% coment_token(StartMatcher,EndMatcher,MatchedSequence)
%%   - match an unrecursive comment outputs the matched sequence used
%%     for building a resulting comment token
%%
%% coment_token_rec(StartMatcher,EndMatcher,MatchedSequence)
%%   - match an recursive comment outputs the matched sequence used
%%     for building a resulting comment token

% will be used as id(Codes) as match Codes under (-->) and used as such in non
% tokenizing comments. Else the extra argument id(Codes,Match) will be
% used as matcher (-->) in tokzing matching, Match==Codes is the matched
% sequence
id(X) --> X.
id(X,X) --> X.

% translate a matcher to a macther or an atom to a matcher that matches the atom
tr(S,SS) :-
    atom(S) ->
        (
            atom_codes(S,C),
            SS=id(C)
        );
    SS=S.

%% comment/2 construction - non recursive non tokenizing matcher
comment_body(E) --> call(E),!.
comment_body(E) --> [_],comment_body(E).
                           
comment(S,E) -->
    {
        tr(S,SS),
        tr(E,EE)
    },
    call(SS),
    comment_body(EE).

%% comment_token/3 construction - non recursive tokenizing matcher
comment_body_token(E,Text) -->
    call(E,HE),!,
    {append(HE,[],Text)}.

comment_body_token(E,[X|L]) -->
    [X],
    comment_body_token(E,L).
                           
comment_token(S,E,Text) -->
    {
        tr(S,SS),
        tr(E,EE)
    },
    call(SS,HS),
    {append(HS,T,Text)},
    comment_body_token(EE,T).

%% comment_token_rec/3 construction - recursive tokenizing matcher

% Use this as the initial continuation, will just tidy up the mathed resul
% with ending the list with [].
comment_body_rec_start(_,_,[]).

comment_body_token_rec(_,E,Cont,Text) -->
    call(E,HE),!,
    {append(HE,T,Text)},
    call(Cont,T).

comment_body_token_rec(S,E,Cont,Text) -->
    call(S,HS),!,
    {append(HS,T,Text)},
    comment_body_token_rec(S,E,comment_body_token_rec(S,E,Cont),T).

comment_body_token_rec(S,E,Cont,[X|L]) -->
    [X],
    comment_body_token_rec(S,E,Cont,L).

comment_token_rec(S,E,Text) -->
    {
        tr(S,SS),
        tr(E,EE)
    },
    call(SS,HS),
    {append(HS,T,Text)},
    comment_body_token_rec(SS,EE,comment_body_rec_start,T).

%% comment_rec/3 construction - recursive non tokenizing matcher
comment_body_rec(_,E) -->
    call(E),!.

comment_body_rec(S,E) -->
    call(S),!,
    comment_body_rec(S,E),
    comment_body_rec(S,E).

comment_body_rec(S,E) -->
    [_],
    comment_body_rec(S,E).

comment_rec(S,E) -->
    {
        tr(S,SS),
        tr(E,EE)
    },
    call(SS),
    comment_body_rec(SS,EE).


%% ============================ testcode ==============================
test(Tok,S,U) :-
    atom_codes(S,SS),
    call_dcg(Tok,SS,U).

test_comment(S) :-
    test(comment('<','>'),S,[]).

test_comment_rec(S) :-
    test(comment_rec('<','>'),S,[]).

test_comment_token(S,T) :-
    test(comment_token('<','>',TT),S,[]),
    atom_codes(T,TT).

test_comment_token_rec(S,T) :-
    test(comment_token_rec('<','>',TT),S,[]),
    atom_codes(T,TT).

start(AA) :-
    (
        catch(b_getval(a,[N,A]),_,N=0) ->
          true;
        N=0
    ),
    NN is N + 1,
    (
        N == 0 ->
          AA = _;
        AA = A
    ),
    b_setval(a,[NN,AA]).

end(A) :-
    b_getval(a,[N,A]),
    NN is N - 1,
    b_setval(a,[NN,A]).
    
a(A) --> 
  {atom_codes(A,AA)},
   AA,
   {start(B)},
   [B].

a(A,C) --> 
  {atom_codes(A,AA)},
   AA,
   {start(B)},
   [B],
   {append(AA,[B],C)}.

b(A) -->
 {end(B)},
 [B],
 {atom_codes(A,AA)},
  AA.

b(A,C) -->
 {end(B)},
 [B],
 {atom_codes(A,AA)},
  AA,
  {append([B],AA,C)}.

test_adapt(S,T) :-
    test(comment_token_rec(a('<'),b('>'),TT),S,[]),
    atom_codes(T,TT).
    

:- multifile test/2.
             
test('Test comment',[true(test_comment('<alla>'))]) :- true.
test('Test comment_rec',[true(test_comment_rec('<alla<balla>>'))]) :- true.
test('Test comment_token',
     [true(A==B)]) :- 
     A='<alla>', 
     test_comment_token(A,B).

test('Test comment_token_rec',
     [true(A==B)]) :- 
   A='<alla<balla>>', 
   test_comment_token(A,B).

test('Test comment_token_rec advanced 1',
     [true(A==B)]) :- 
   A='<1 alla2> <1 balla2> 1>1>', 
   test_adapt(A,B).

test('Test comment_token_rec advanced 2',
     [true(A==B)]) :- 
   A='<2 alla1> <2 balla1> 2>2>', 
   test_adapt(A,B).

    
        
