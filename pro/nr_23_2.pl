/**
 Ponta vortaro en kvin paŝoj

    1. Enkonduko en Prologon
 => 2. Legi vortaron, rilatigi vortojn
    3. Tradukparoj kaj -pontoj
    4. Esperanto-ĉina vortaro
    5. Servi retpaĝon

*/

:- use_module(library(csv)).
:- dynamic traduko/5.


/**
 * Enlegi la tradukojn el CSV-dosiero
 * poste ili estas pridemandeblaj per: 
 * ?- traduko(Eo,Mrk,Lng,Trd,Ind).
 */ 
legu :- 
    csv_read_file('vrt/eo_de_en_fr_nl.csv', Datenopoj, [
        separator(0';),   % uzu punktokomon kiel apartigilo de kampoj
        skip_header('#'), % ignoru kaplinion, enkondukitan per #
        convert(false),   % ne provu interpreti nombrojn ks.
        functor(traduko)  % nomo de la predikato (~ tabelnomo)
    ]),
    % ni ricevis liston, kies unuopajn erojn
    % ni nun konservas kiel faktoj (per "assert")
    maplist(assert, Datenopoj).


trd(Lng,Eo,Nac) :-
    traduko(Eo,_,Lng,Trd,Ind),
    (Trd\='' -> Nac=Trd; Nac=Ind).
    
/**
 * trovas (verŝajnajn) sinonimojn per samajn tradukoj en iu lingvo de du diversaj
 * esperanto-vortoj: sin1(L1,malsanulejo,E2).
 * Sed tio ne estas perfekta, ĉar en ekz-e droneo kaj virabelo
 * havas en la germana ambaŭ la tradukon "Drohne": sin1(L,virabelo,E2).
 */
sin1(Lng,Eo1,Eo2) :-
    distinct(Eo1-Eo2,(
        trd(Lng,Eo1,Nac),
        trd(Lng,Eo2,Nac),
        Eo1 \= Eo2
    )).

/**
 * Ni provas plibonigi sin1 komparante almenaŭ du diversajn lingvojn.
 * Nun ni ne plu ricevas sinonimojn virabelo-droneo: sin2(L1,L2,virabelo,E2).
 * Sed ankaŭ ne plu malsanulejo-hospitalo: sin2(L1,L2,malsanulejo,E2).
 * Kial?
 */
sin2(Lng1,Lng2,Eo1,Eo2) :-
    distinct(Eo1-Eo2,(
        trd(Lng1,Eo1,Nac),
        trd(Lng1,Eo2,Nac),
        Eo1 \= Eo2,
        trd(Lng2,Eo1,Nac),
        trd(Lng2,Eo2,Nac),
        Lng1 \= Lng2
    )).