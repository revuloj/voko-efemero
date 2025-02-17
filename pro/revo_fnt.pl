:- use_module(library(sgml)).
:- use_module(library(xpath)).

:- dynamic radiko/3.

revo_xml('./xml/*.xml').

/**
Ekstraktas la radikkon kune kun fontindikoj el Revo-artikoloj sub ./xml
*/


revo_fnt :-
  revo_trasercho, 
  skribu.

%! revo_trasercho is det.
%

revo_trasercho :-
  retractall(radiko(_,_)),

  revo_xml(Xml),
%  atom_concat(Pado,'/*.xml',XMLDosieroj),
  expand_file_name(Xml,Dosieroj),
 
  forall(
      member(Dosiero,Dosieroj),
      catch(
        (
          % format('~w -> ',[Dosiero]),
          once((
            revo_art(Dosiero)
            ;
            throw(eraro(ne_analizita))
          ))
        ),
        Exc,
        handle_exception(Dosiero,Exc)
      )
   ).

%! skribu is det.
%

skribu :-
  skribu_radikojn.


skribu_radikojn :-
  radik_dosiero(Dos),
  format('skribas al ''~w''...~n',[Dos]),
  setup_call_cleanup(
    open(Dos,write,Out),
    skribu_radikojn(Out),
    close(Out)
  ).

skribu_radikojn(Out) :-
  setof(
    Rad-(Ofc,Spec), % ordigu oficialecon antaŭ vortspeco por havi '' post la oficialaj!
    radiko(Rad,Spec,Ofc),
    Chiuj),
  %keysort(Chiuj,Ordigitaj),
  reverse(Chiuj,Renversitaj),
  forall(
    member(R-(O_,S),Renversitaj), % renversu por ke "senil" analiziĝu antaŭ "sen/il" ktp.
    % format(Out,'r(''~w'',~w) --> "~w".~n',[R,S,R])
    (
      vrt_ofc(O_,O),
      once((
        r(R,S,O) % se estas en la baza vortaro ne skribu al revo-vortaro
        ;
        format(Out,'r(~q,~q,~q).~n',[R,S,O]) 
      ))
    )
  ).



handle_exception(Dosiero,Exception) :-
  once(
    (
      Exception = eraro(Eraro), format('~w -> ERARO: ~w~n',[Dosiero,Eraro]);
      Exception = averto(Averto), format('~w -> AVERTO: ~w~n',[Dosiero,Averto])
    )
  ).

%! revo_art(+Dosiero).
%
% Trakuras XML-Revo-artikolon (DOM) kaj elkribras la
% bezonatajn informojn kiel radikon, vortspecon kaj mallongigojn.
% La rezulto estas faktoj, el kiuj poste kreiĝos la vortlistoj.
% Evitindaj radikoj kaj nomradikoj estas aparte traktitaj.

revo_art(Dosiero) :-
  load_xml_file(Dosiero,DOM),
  catch(
    (
      revo_rad(DOM,Radiko,Ofc),!, % enestu nur unu, 
                % do ni ne plu serĉas aliajn radikojn //art/kap/rad 

      % ne jam preta, teste... var - TIEL NI TROVOS NUR UNU var! sed foje enestas du!
      once((
        revo_var(DOM,VarRad,VOfc) %, format('DBG var: ~w: ~w~n',[Dosiero,VarRad])
        ; true
      ))
          
      % format('~w (~w)~n',[Radiko,Speco]),
    ),
    Exc,
    (
      throw(Exc)
    )
  ),

  % memoru la rezulton de la analizo kiel faktoj
  assert_vorto(DOM,Radiko,Speco,Ofc),
  (nonvar(VarRad) -> assert_vorto(DOM,VarRad,Speco,VOfc); true).


assert_vorto(DOM,Radiko,Speco,Ofc) :-
  %%assert_radiko(DOM,Radiko,Speco),
  ofc_vrt(Ofc,OVrt),!,
  assertz(radiko(Radiko,Speco,OVrt)).


revo_rad(DOM,Radiko,Ofc) :-
  xpath(DOM,//art/kap,Kap),
  xpath(Kap,rad(normalize_space),Radiko),
  atom_length(Radiko,L), 
  %(L=<1 
  %  -> throw(averto('ellasante unuliteran radikon')) % ne akceptu radikojn unuliterajn
  %  ; true
  %),

  % ni bezonos la unua derivaĵon en la artikolo
  % por analizi EVI, vorspecon, klasojn
  xpath(DOM,//drv(1),Drv),

  % eltrovu la oficialecon
  once((
    xpath(Kap,ofc(normalize_space),Ofc)
    ;
  % evitindecon de neoficialaj vortoj (en drv[1] aŭ drv[1]/snc[1] ni tie notas kiel 'e')
    xpath(Drv,uzo(@tip=stl,text),'EVI'),
    Ofc = 'e'
    ;
    xpath(Drv,snc(1)/uzo(@tip=stl,text),'EVI'),
    Ofc = 'e'
    ;
    % nek oficiala nek evitinda:
    Ofc=''
  )).


revo_var(DOM,VarRad,Ofc) :-
  xpath(DOM,//art/kap/var/kap,Kap),
  xpath(Kap,rad(normalize_space),VarRad),
  atom_length(VarRad,L), 
  (L=<1 
    -> throw(averto('ignoras unuliteran radikon')) % ne akceptu radikojn unuliterajn
    ; true
  ),

  % eltrovu la oficialecon
  once((
    xpath(Kap,ofc(normalize_space),Ofc)
    ;
%%    % PLIBONIGU: la sekvan ni jam faras en revo_rad, ĉu ni fakte ripetu tie ĉi?
%%    % evitindecon de neoficialaj vortoj (en drv[1] aŭ drv[1]/snc[1] ni tie notas kiel 'e')
%%    xpath(DOM,//drv(1),Drv),
%%    (
%%      xpath(Drv,uzo(@tip=stl,text),'EVI');
%%      xpath(Drv,snc(1)/uzo(@tip=stl,text),'EVI')
%%    ),
%%    Ofc = 'e'
%%    ;
%%    % nek oficiala nek evitinda:
    Ofc=''
  )).


% ĉar ni inverse ordigos la vortaron ni
% uzos signojn por oficialeco, kiuj en
% Askio estas en la dezirata loko:
% ! = evitinda
% + = vorto neoficiale aldonita
% 1-9, 1953 ks = per oficiala aldono
% F = fundamenta
ofc_vrt('*','F').
ofc_vrt('','+').
ofc_vrt('e','!').
ofc_vrt(O,O) :- nonvar(O).
ofc_vrt(_,'+').

% retraduki F -> * antaŭ sekurigi!
vrt_ofc('F','*').
vrt_ofc(O,O).

    
test(X) :- 
 xpath(element(kap, [], ['\n  ', element(rad, [], [abaĵur]), '/o ', element(fnt, [], [element(bib, [], [...])]), '\n']), self, X).

  
