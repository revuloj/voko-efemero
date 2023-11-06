% faktoj pri Prologo
% pridemandebla per ?- speco('Prologo',X)
speco('Prologo',programlingvo).
speco('Prologo',solvilo).
speco('Prologo',datumbazo).
speco('Prologo',serĉilo).

% faktoj pri lingvoj
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
 */
besto2(B) :- 
    besto(B).
besto2(B) :-
    speco(B,B1),
    besto2(B1).