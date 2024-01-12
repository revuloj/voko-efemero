
:- use_module(library(dcg/basics)).

%test :-
%    atom_codes('<?xml version="1.0" encoding="UTF-8"?><!ENTITY amacron "&#257;"><!ENTITY emacron "&#275;">',Codes),
%    dtd(Codes).
%

%test :-
%    t(4,[`Alfa`-`&#x391;`,`alfa`-`&#x3b1;`,`alfa_acute`-`&alfa_ton;`,
%        `alfa_subj`-`&alfa;`,`Alfa_Subj`-`&Alfa;`,`Alfa_ton`-`&#x0386;`,`alfa_ton`-`&#x03ac;`]).
    
    
test(T) :-
      phrase(dtd(T),`<!ENTITY amacron "&#257;">`),
      member(entity(N-V),T),
      format('entity: ~s - ~s~n',[N,V]).

dtd([]) --> eos.

dtd([H|T]) -->
  decl(H), !,
%  { format('decl: ~w~n',[H]) },
% { portray_clause(H) },
  dtd(T).


decl(entity(Name-Value)) -->
  "<!ENTITY", blanks, name(Name), blanks, """", value(Value), """>", blanks.

decl(param(Name-Value)) -->
  "<!ENTITY", blanks, "%", blanks, name(Name), blanks, """", value(Value), """>", !, blanks.


decl(exception) -->
  "<!ENTITY", !, string_without(">",S), { atom_codes(A,S), throw(could_not_parse_entity(A)) }.


name(Name) --> namesigns(Name). %, { atom_codes(Name,Letters) }.

value(Value) --> string_without(""">",Value). %, { atom_codes(Value,Codes) }.



namesigns([H|T]) -->
        [H],
        { code_type(H, csym)
        }, !,
        namesigns(T).
namesigns([H|T]) -->
        "-", !,
        { char_code('-',H) },
        namesigns(T).
/*
namesigns([H|T]) -->
        "_", !,
        { char_code('_',H) },
        namesigns(T).
*/
namesigns([]) -->
        [].



parse_error_(Msg,Rest,Rest) :- !,
  once(( % ne kreu saman eraron diversmaniere...
        first_part_of_list(20,Rest,Begin),
        format('~w: ~s~n',[Msg,Begin]),
        throw(Msg)
%        assertz(dtd_parse_error(Msg,Begin))
  )).

first_part_of_list(Count,List,List) :-
  length(List,ListLen),
  ListLen < Count.

first_part_of_list(Count,List,Head) :-
  length(Head,Count),
  append(Head,_,List).



