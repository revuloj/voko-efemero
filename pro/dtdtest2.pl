
:- use_module(library(dcg/basics)).

/****

From XML standard https://www.w3.org/TR/2008/REC-xml-20081126/

EntityValue	   ::=   	'"' ([^%&"] | PEReference | Reference)* '"'
 |  "'" ([^%&'] | PEReference | Reference)* "'"

Entity Reference

[67]   	Reference	   ::=   	EntityRef | CharRef
[68]   	EntityRef	   ::=   	'&' Name ';'	[WFC: Entity Declared]
[VC: Entity Declared]
[WFC: Parsed Entity]
[WFC: No Recursion]
[69]   	PEReference	   ::=   	'%' Name ';'	[VC: Entity Declared]
[WFC: No Recursion]
[WFC: In DTD]

NameStartChar	   ::=   	":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
[4a]   	NameChar	   ::=   	NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
[5]   	Name	   ::=   	NameStartChar (NameChar)*


[66]   	CharRef	   ::=   	'&#' [0-9]+ ';'
| '&#x' [0-9a-fA-F]+ ';'	[WFC: Legal Character]


****/

test(Parsed) :-
    phrase(char_ref(Parsed),`&#257;`).

misoj :-
  forall(
  member(N,[257, 275,291,299,311,316,326, 333, 343, 363, 256,274,290,298,310,315,325,332,342,362]),
  (
  number_codes(N,C),
  dec_mis_(C,_,Mis),
  format('~16r,~w,~w~n',[Mis,Mis,N])
  )).

dec_mis_([H|T],Fact,Val) :-
  dec_mis_(T,F,V),
  Val is (H) * F + V,
  Fact is F*10.

dec_mis_([],1,0).
%dec_value_([H|T],Fact,Val) :-
%  dec_value_(T,F,V),
%  Val is H * F + V,
%  Fact is F*10.
  
  dec_value_([],1,0).

char_ref(C) --> "&#x", xdigits(HexDigs), ";", { hex_chr(HexDigs,C) }.
char_ref(C) --> "&#", digits(Digits), ";", { dec_chr(Digits,C) }.


hex_chr(Hex,Val) :-
  hex_value_(Hex,_,Val).

dec_chr(Dec,Val) :-
  dec_value_(Dec,_,Val).

hex_value_([H|T],Shift,Val) :-
  hex_value_(T,Sh,V),
  Val is H << Sh \/ V,
  Shift is Sh + 4.

hex_value_([],0,0).

dec_value_([H|T],Fact,Val) :-
  dec_value_(T,F,V),
  Val is (H-0'0) * F + V,
  Fact is F*10.

dec_value_([],1,0).