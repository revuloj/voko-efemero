/**
 Ponta vortaro en kvin paŝoj

 1. Enkonduko en Prologon
 2. Legi vortaron, rilatigi vortojn
 3. Tradukparoj kaj -pontoj
 4. Esperanto-ĉina vortaro
 5. Servi retpaĝon

*/


% faktoj pri Prologo k.a.
% pridemandebla per ?- speco('Prologo',X)
% SQL-ekvivalento: 
% SELECT * FROM speco WHERE subjekto = 'Prologo';
%
% (variablo-nomoj estas majusklaj, do tekstvaloroj
% estu aŭ minusklaj aŭ intercitilaj)
speco('Prologo',programlingvo).
speco('Prologo',solvilo).
speco('Prologo',datumbazo).
speco('Prologo',serĉilo).
speco('Paskalo',programlingvo).
speco('Pitono',programlingvo).
speco('Perlo',programlingvo).
speco('Orakolo',datumbazo).

% faktoj pri lingvoj
% la sintakso en SQL estus
% INSERT INTO speco VALUES ('angla,'nacilingvo'); -- ktp.
speco(angla,nacilingvo).
speco(franca,nacilingvo).
speco(germana,nacilingvo).
speco(nederlanda,nacilingvo).
speco('Esperanto',internacilingvo).

% faktoj pri bestoj
speco(hundo,rabobesto).
speco(rabobesto,mamulo).
speco(mamulo,besto).
speco(dogo,hundo).
speco(urso,rabobesto).


/**
 * ?- speco(L,lingvo) ne donas rezulton, manke de faktoj, sed ni
 * povas difini regulon:
 * Lingvo povas esti nacilingvo aŭ internacilingvo aŭ programlingvo
 *
 * SQL-ekvivalento:
 * SELECT subjekto FROM speco WHERE speco = 'nacilingvo'
 *     OR speco = 'internacilingvo' OR speco = 'programlingvo'
 */
lingvo(L) :-
    speco(L,nacilingvo)
    ;
    speco(L,internacilingvo)
    ;
    speco(L,programlingvo).

/**
 * Ĉio, de la speco besto estas besto.
 */
besto(B) :-
    speco(B,besto).

/*
 * provu:
 * ?- besto(mamulo).
 * ?- besto(hundo).
 * ?- besto(B).
 */ 

/**
 * Ankaŭ ĉiuj mamuloj, hundoj ktp. estas bestoj. Ni bezonas pli vastan
 * difino por besto do:
 * Ĉio, kies speco siavice estas besto, estas besto (rikura difino)
 * Kontrola demando: kial la rikuro funkcias? Kial ĝi ne kuras eterne?
 */
besto2(B) :- 
    besto(B).
besto2(B) :-
    speco(B,B1),
    besto2(B1).