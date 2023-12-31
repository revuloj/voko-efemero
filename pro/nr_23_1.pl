/**
 Ponta vortaro en kvin paŝoj

 => 1. Enkonduko en Prologon
    2. Legi vortaron, rilatigi vortojn
    3. Tradukparoj kaj -pontoj
    4. Esperanto-ĉina vortaro
    5. Servi retpaĝon

*/

% En Prologo oni laboras per predikatoj, kiuj povas esti faktaroj aŭ regularoj.
% Sed por programistoj pli facila estas analogio al datumbazo:
% faktaro ~ tabelo (en: table), fakto ~ rikordo (en: record; row), regulo ~ rigardo (en: view)

% Ni komencu per difino de kelkaj faktoj pri Prologo k.a.
% pridemandeblaj per ?- speco('Prologo',X)
%
% SQL-ekvivalento:
% INSERT INTO speco VALUES ('Prologo','programlingvo'); -- ktp. 
% SELECT * FROM speco WHERE subjekto = 'Prologo';
%
% Por tio, kion oni serĉas oni uzas variablojn, por
% konataj valoroj minusklaj aŭ citilitaj 'atomoj':
% (Ĉar variablo-nomoj estas majusklaj, do tekstvaloroj
% estu aŭ minusklaj aŭ intercitilaj)
%
% La aparta variablo '_' (substreko), uziĝas por iaj neinteresaj
% (ignorataj) valoroj
speco('Prologo',programlingvo).
speco('Prologo',solvilo).
speco('Prologo',datumbazo).
speco('Prologo',serĉilo).
speco('Paskalo',programlingvo).
speco('Pitono',programlingvo).
speco('Perlo',programlingvo).
speco('Orakolo',datumbazo).

% Ni aldonu kelkajn faktojn pri lingvoj.
speco(angla,nacilingvo).
speco(franca,nacilingvo).
speco(germana,nacilingvo).
speco(nederlanda,nacilingvo).
speco('Esperanto',internacilingvo).

% kaj faktojn pri bestoj:
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
 * CREATE VIEW lingvo AS SELECT subjekto FROM speco WHERE speco = 'nacilingvo'
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