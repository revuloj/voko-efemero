:- use_module(library(sgml)).
:- use_module(library(xpath)).

:- dynamic radiko/2.

revo_xml('./xml/*.xml').
radik_dosiero('vrt/revo_rad.pl').

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
  forall(
    radiko(R,F),
    format(Out,'r(~q,~q).~n',[R,F]) 
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
      revo_rad(DOM,Radiko,Fnt),!, % enestu nur unu, 
                % do ni ne plu serĉas aliajn radikojn //art/kap/rad 

      % ne jam preta, teste... var - TIEL NI TROVOS NUR UNU var! sed foje enestas du!
      once((
        revo_var(DOM,VarRad,VFnt) %, format('DBG var: ~w: ~w~n',[Dosiero,VarRad])
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
  assertz(radiko(Radiko,Fnt)),
  (nonvar(VarRad) -> assertz(radiko(VarRad,VFnt)); true).


revo_rad(DOM,Radiko,Fnt) :-
  xpath(DOM,//art/kap,Kap),
  xpath(Kap,rad(normalize_space),Radiko),

  % eltrovu la oficialecon
  once((
    xpath(Kap,fnt/bib(normalize_space),Fnt)
    ;
    % ne havas vnt/bib
    Fnt=''
  )).


revo_var(DOM,VarRad,VFnt) :-
  xpath(DOM,//art/kap/var/kap,Kap),
  xpath(Kap,rad(normalize_space),VarRad),

  % eltrovu la oficialecon
  once((
    xpath(Kap,fnt/bib(normalize_space),VFnt)
    ;
    VFnt=''
  )).

  
test(X) :- 
 xpath(element(kap, [], ['\n  ', element(rad, [], [abaĵur]), '/o ', element(fnt, [], [element(bib, [], [...])]), '\n']), self, X).

  
