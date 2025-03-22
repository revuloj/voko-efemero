:- use_module(library(sgml)).
:- use_module(library(xpath)).
:- use_module(library(http/json)).

:- dynamic bl/4.

bl_file('./vrt/bonalingvo.html').


read_bl :-
    bl_file(BL),
    load_html(BL,DOM,[]),
    parse_bl(DOM).


parse_bl(DOM) :-
    retractall(bl(_,_,_,_)),
    xpath(DOM,//div(@id=tuta)/dl,element(dl,_,Difinoj)),
    parse_dt_dd(Difinoj),!.
    
/**
    <dl>
    <dt> <b>-ab-</b> <i>(-ab-)</i> :</dt><br>
    <dd> ankaŭ eblas diri '-instalaĵ-' <i>(-instal·aĵ-)</i>, '-uj-' <i>(-uj-)</i>.</dd><br><br>
    <dt> <b>-al</b> <i>(-al)</i> :</dt><br>
    <dd>(se temas pri ostoj aŭ per aliaj vortoj laŭ la signifo en la unuopaj okazoj) prefere diru '-osto'
        <i>(-ost·o)</i>.</dd><br><br>
    ...
**/        

parse_dt_dd([]).
parse_dt_dd([element(dt,_,DT),element(dd,_,DD)|Difinoj]) :-
    %assertz(bl(DT,DD)),
    parse_entry(DT,DD),
    parse_dt_dd(Difinoj).

parse_entry(DT,DD) :-
    parse_dt(DT,Kap,Dis,Fnt),
    assertz(bl(Kap,Dis,Fnt,DD)).

parse_dt(DT,Kap,Dis,Fnt) :-
    % kapvorto
    select(element(b,_,[Kap]),DT,DT1),
    % element-dismeto
    once((
        select(element(i,_,[Dis]),DT1,DT2)
        ;
        Dis='', DT2=DT1
        )),
    % fonto
    once((
        select(F,DT2,_),
        atom(F),
        sub_atom(F,B,1,_,']'),
        B1 is B-1,
        sub_atom(F,1,B1,_,Fnt)
        ;
        Fnt=''
    )).

% parse_dd(DD,[]) :-


