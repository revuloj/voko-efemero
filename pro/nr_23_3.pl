:- use_module(library(csv)).
:- use_module(library(isub)).

:- dynamic eo/5, trd/3.

%csv_hande('vrt/handedict_nb.u8').
csv_hande('vrt/handedict_23.u8').

% ŝanĝu sufikson _a al alia litero kiam vi prilaboras aliajn literojn!
csv_mankoj('vrt/eo_de_g.csv').
db_celo('pdb/eo_zh_g.db').
csv_celo('vrt/eo_zh_g.csv').

legu :-
    legu_eo('vrt/eo_de_en_fr_nl.csv'),
    triopoj(eo),
    legu_zh(de,'vrt/handedict_23.u8'),
    legu_zh(en,'vrt/cedict.u8'),
    triopoj(zh).


/**
 * Enlegi la tradukojn de esperantaj vortoj el CSV-dosiero
 * poste ili estas pridemandeblaj per: 
 * ?- eo(Eo,Mrk,Lng,Trd,Ind).separator(0'/),ignore_quotes(true),convert(false),skip_header('#'),match_arity(false)
 */ 
legu_eo(EoCsv) :- 
    format('legante ~w...~n',EoCsv),
    csv_read_file(EoCsv, Datenopoj, [
        separator(0';),   % uzu punktokomon kiel apartigilo de kampoj
        skip_header('#'), % ignoru kaplinion, enkondukitan per #
        convert(false),   % ne provu interpreti nombrojn ks.
        functor(eo)       % nomo de la predikato (~ tabelnomo)
    ]),
    % ni ricevis liston, kies unuopajn erojn
    % ni nun konservas kiel faktoj (per "assert")
    maplist(assert, Datenopoj).


/**
 * Enlegi la tradukojn de germanaj vortoj al la ĉina el HandeDict
 * vd https://handedict.zydeo.net/de/download
 * poste ili estas pridemandeblaj per: 
 * ?- traduko(Eo,Mrk,Lng,Trd,Ind).
 */ 
legu_zh(Lng,ZhDict) :- 
    format('legante ~w...~n',ZhDict),
    csv_read_file(ZhDict, Datenopoj, [
        separator(0'/),      % uzu oblikvon kiel apartigilo de kampoj
        ignore_quotes(true), % ignoru citilojn - ili ne servas por kadri kampotekstojn
        convert(false),      % ne provu interpreti nombrojn ks.
        skip_header('#'),    % ignoru kaplinion, enkondukitan per #
        functor(zh),         % nomo de la predikato (~ tabelnomo)
        match_arity(false)   % linioj havas unu ĉinan kolumnon kaj unu aŭ plurajn germanajn
    ]),
    % ni ricevis liston, kies unuopajn erojn
    % ni nun konservas kiel faktoj (per "assert"):
    % zh(Lng,Zh,Trd1,...,Trdn).
    maplist(assert_zh(Lng),Datenopoj).

assert_zh(Lng,Datenopo) :-
    Datenopo =.. [_|Kampoj],
    Fakto =.. [zh,Lng|Kampoj],
    assert(Fakto).

triopoj(eo) :-
    forall(
        eo(Eo,_,Lng,Trd,Ind),
        (
        Trd \= '' -> assertz(trd(eo-Lng,Eo,Trd)) % - se Trd ne malplena -
        ; assertz(trd(eo-Lng,Eo,Ind))
        )
    ).

/*
 * HanDeDict havas inter 3 kaj 22 kampojn - lasta kutime malplena
 * ni transformas tion al duopoj ĉina - germana
 */
triopoj(zh) :-
    forall(
    (
        between(4,23,An),
        functor(F,zh,An),   % F = zhde(_,_,..._) kun 3 ĝis 22 argumentoj
        catch(F,_,true),    % pridemandu la faktojn zhde(...) ignorante erarojn (catch anst. call)
        F =.. [_,Lng,Zh|Trdj],  % transformu al listo [zhde,Ĉina,De_1,...,De_n]
        member(Trd,Trdj),   % por ĉiu germanlingva kampo 
        Trd \= ''           % - se ne malplena -
    ),
        assertz(trd(zh-Lng,Zh,Trd)) % kreu nun apartan fakton kiel paro zhde(Ĉina,Germana)
    ).

/*
 * Permesu en la triopoj serĉi tradukojn inter du lingvoj laŭ ambaŭ direktoj kaj
 * aldone tra pontlingvo
 */
trd2(Lde-Lal,De,Al) :-
    trd(Lde-Lal,De,Al). % traduko estas en la faktoj trd/3

trd2(Lde-Lal,De,Al) :-
    trd(Lal-Lde,Al,De). % traduko estas inverse trovebla en la faktoj trd/3


/**
 * Nun ni ebligos trovi tradukojn, por kiuj ne nepre
 * havas paron en niaj faktoj. Tiam ni provos uzi
 * pontlingvon (eo aŭ zh povas funkcii nun), ekz-e:
 * traduko(de-en,dürftig,En,Pont).
 * traduko(eo-zh,ekzemplo,Zh,Pont).
 */ 
    
traduko(Lde-Lal,De,Al,'-':'-') :-
    trd2(Lde-Lal,De,Al). % traduko estas en la faktoj trd2/3 (ambaŭdirekta)   

traduko(Lde-Lal,De,Al,Lp:Ponto) :-
    trd2(Lde-Lp,De,Ponto), % ni povas troci tradukon per pontlingvo
    trd2(Lp-Lal,Ponto,Al).

/**
 * eo_zh(ekzemplo,Zh,Ponto).
 */
eo_zh(Eo,Zh,1.0-Lp:Ponto) :-
    member(Lp,[en,de]),
    trd(eo-Lp,Eo,Ponto), % ni povas trovi tradukon per pontlingvo
    trd(zh-Lp,Zh,Ponto).

eo_zh(Eo,Zh,Simileco-Lp:P2) :-
    member(Lp,[en,de]),
    trd(eo-Lp,Eo,P1), % ni povas trovi tradukon per pontlingvo kaj simileco
    trd(zh-Lp,Zh,P2),
    isub(P1,P2,Simileco,[zero_to_one(true)]),
    Simileco>0.8.

% Ni iom simpligas la dialogon 
% permesante doni komencan demandsignon kaj serĉvorton,
% do ?<serĉvorto>. anstataŭ p(<serĉvorto>).
% :- op(800,fy,user:(?)).
% ?(Eo) :- p(Eo).
% ?(Eo-De) :- pde(Eo,De).




