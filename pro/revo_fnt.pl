:- use_module(library(sgml)).
:- use_module(library(xpath)).
:- use_module(library(http/json)).

:- dynamic radiko/2, fde/1, jed/1, pvee/1.

revo_xml('./xml/*.xml').
fde_file('./vrt/fundamento.json').
jed_file('./vrt/juerg_eo_de.json').
% vd https://arkivo.esperanto-france.org/divers/pvee/pvee.htm
pvee_file('./vrt/pvee_a-z_utf8.html').

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


read_fde :-
  fde_file(JF),
  open(JF,read,Stream,[]),
  json_read(Stream,json(FdE)),
  parse_fde(FdE),
  close(Stream).

read_jed :-
  jed_file(JF),
  open(JF,read,Stream,[]),
  json_read(Stream,json(JED)),
  parse_jed(JED),
  close(Stream).

read_pvee :-
  pvee_file(XV),
  load_html(file(XV),DOM,[]),
  parse_pvee(DOM).

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


parse_fde(JList) :-
  retractall(jed(_)),
  forall(
    member(V=Ref,JList),
    % TODO: asertu nur tiujn, kiu havas iun UV... en Ref
    (      
      % kaj forigu la parton post \' (finaĵon)    
      norm_rad(V,'''',Rad),
      assertz(fde(Rad))
    )
  ).


parse_jed(JList) :-
  retractall(jed(_)),
  forall(
    member(V=_,JList),
    (
      % TODO: forigu komencan '-'?
      % forigu finan -a, -o, -i
      norm_rad(V,'-',Rad),
      assertz(jed(Rad))
    )
  ).

parse_pvee(DOM) :-
  retractall(pvee(_)),
  forall(
    xpath(DOM,//dt/a(normalize_space),A),
    (
      norm_rad(A,'’',Rad),
      assertz(pvee(Rad))
    )
  ).

norm_rad(Vrt,Sep,Rad) :-
  downcase_atom(Vrt,Min),
  once((
    atomic_list_concat([Rad,F],Sep,Min),
    member(F,[a,o,i,e])
    ;
    atomic_list_concat([Rad,''],Sep,Min)
    ;
    Rad = Min
  )).
  
test(X) :- 
 xpath(element(kap, [], ['\n  ', element(rad, [], [abaĵur]), '/o ', element(fnt, [], [element(bib, [], [...])]), '\n']), self, X).

  
