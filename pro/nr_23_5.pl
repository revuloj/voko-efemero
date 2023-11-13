:- use_module(library(csv)).
:- use_module(library(isub)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).

:- multifile http:location/3.
:- dynamic eo/5, trd/3, http:location/3.

%csv_hande('vrt/handedict_nb.u8').
csv_hande('vrt/handedict_23.u8').

% ŝanĝu sufikson _a al alia litero kiam vi prilaboras aliajn literojn!
csv_mankoj('vrt/eo_de_g.csv').
db_celo('pdb/eo_zh_g.db').
csv_celo('vrt/eo_zh_g.csv').

user:file_search_path('ret','./ret').
http:location(sercho,root(sercho),[]).

:- initialization((legu, start_server(3333))). 
:- http_handler(root(sercho), sercho, []). 
:- http_handler(root(.), http_reply_from_files(ret(.),[]),[prefix]).

start_server(Port) :-
    format('lanĉante serĉilon ĉe http://0.0.0.0:~w~n',Port),
    http_server(http_dispatch, [port(Port)]).

sercho(Request) :-
    debug(http(sercho),'Serĉo ~q',[Request]),
    http_read_json(Request, json(JSON)),
    %format('Content-type: text/plain~n~n'),
    member(q=Sercho,JSON),
    with_output_to(atom(Trovoj),zh_tradukoj(Sercho,20),[]),
    reply_json(json([trovoj=Trovoj])).

legu :-
    legu_eo('vrt/eo_de_en_fr_nl.csv'),
    triopoj(eo),
    legu_zh(de,'vrt/handedict_23.u8'),
    legu_zh(en,'vrt/cedict.u8'),
    legu_zh(fr,'vrt/cfdict.u8'),
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

/**
 * eo_zh(ekzemplo,Zh,Ponto).
 */
eo_zh(Eo,Zh,1.0-Lp:Ponto) :-
    member(Lp,[en,de,fr]),
    trd(eo-Lp,Eo,Ponto), % ni povas trovi tradukon per pontlingvo
    trd(zh-Lp,Zh,Ponto).

eo_zh(Eo,Zh,Simileco-Lp:P2) :-
    member(Lp,[en,de,fr]),
    limit(100,(
        trd(eo-Lp,Eo,P1), % ni povas trovi tradukon per pontlingvo kaj simileco
        trd(zh-Lp,Zh,P2),
        P1 \= P2, % evitu duoble trovi/kalkuli tradukojn
        isub(P1,P2,Simileco,[zero_to_one(true)]),
        Simileco>0.8
    )).

eo_zh_grup(Eo,Zh,SimSum-Pontoj) :-
    % serĉu parojn Eo-Zh 
    % kaj identajn paroj sumiĝu
    group_by(Eo-Zh,
        Simil-Lp:Ponto,
        eo_zh(Eo,Zh,Simil-Lp:Ponto),
        Trovoj),
    % ni nun adicias la similecojn al poentoj, por samaj paroj
    foldl(kunigo,Trovoj,0-[],SimSum-Pontoj).

kunigo(
    Sim1-Lp1:P1,
    Sim2-PList,
    SimSum-[Lp1:P1|PList]) :- 
        SimSum is Sim1+Sim2.

eo_zh_ord(Eo,Zh,SimSum-Pontoj) :-
    order_by([desc(SimSum)], 
        eo_zh_grup(Eo,Zh,SimSum-Pontoj)
    ).

zh_pontoj(Eo,Kiom) :-
    forall(
    limit(Kiom,eo_zh_ord(Eo,Zh,SimSum-Pontoj)),
    format('~1f ~w ~36|~w~n',[SimSum,Zh,Pontoj])
    ).

zh_trdj(Zh,Tradukoj) :-
    setof(Lng:Trd,trd(zh-Lng,Zh,Trd),Tradukoj).

zh_tradukoj(Eo,Kiom) :-
    forall(
        limit(Kiom,eo_zh_ord(Eo,Zh,SimSum-_)),
        (
            zh_prononco(Zh,ZhPr),
            format('~1f ~w~n',[SimSum,ZhPr]),
            zh_trdj(Zh,Tradukoj),
            forall(
                group_by(Lng, Trd, member(Lng:Trd,Tradukoj), LngTrd),
                (
                    atomic_list_concat(LngTrd,'; ',TrdStr),
                    %format('          ~w: ~w~n',[Lng,TrdStr])
                    format('~7|~w: ~w~n',[Lng,TrdStr])
                )
            )
        )
    ).

% traduku prononcon, troviĝantan inter angulaj krampoj 
% de ciferaj sufiksoj al supersignaj
zh_prononco(Zh, ZhPr) :-
    % elprenu la prononcon el inter rektaj krampoj
    sub_atom(Zh,K1,1,_,'['),
    sub_atom(Zh,K2,1,_,']'),
    K2>K1, 
    K1_ is K1 + 1,
    KL is K2 - K1 - 1,
    sub_atom(Zh,K1_,KL,_,Pr),
    % apartigu silabojn
    atomic_list_concat(Silab,' ',Pr),
    % transformu prononcon
    zh_prononco_s(Silab,SilabS),
    % kunigu silabojn
    atomic_list_concat(SilabS,' ',PrS),
    % remetu transformitan prononcon
    sub_atom(Zh,0,K1,_,Ant),
    % ni ne atendu tekston post la prononco, cu?
    % sub_atom(Zh,K2,_,0,Post),

    % forigu tradukojn duoblajn metu komojn
    zh_listo(Ant,ZhPr,PrS),!. 

zh_listo(L,L1,Pr) :-
    atomic_list_concat(Tj,' ',L),
    % forigu evtl. duoblajn
    setof(T1,
        (
            member(T,Tj), 
            normalize_space(atom(T1),T), 
            T1\= ''
        ),
        T1j),
    % aldonu prononcon al ĉiu unuopa
    normalize_space(atom(PrN),Pr),
    maplist(zh_map_pr(PrN),T1j,T2j),
    % ĉenigu kun interaj komoj
    atomic_list_concat(T2j,',',L1).

zh_map_pr(Pr,Zh,ZhPr) :-
    atomic_list_concat([Zh,'[',Pr,']'],ZhPr).   

zh_prononco_s([],[]).
zh_prononco_s([S1|Rest],[SS1|RestS]) :-
    atom_chars(S1,SD),
    once((
        % silabo konsistas el askiaj literoj kaj fina cifero
        append(Silab,[C],SD), 
        atom_number(C,D)
        ; % se ne estas cifero supozu neŭtralan tonon (5)
        D=5,
        Silab=SD
    )),
    zh_pr_vokal(Silab,0,Paroj),
    zh_pr_silab(Paroj,D,SS),
    atom_chars(SS1,SS),
    zh_prononco_s(Rest,RestS).

% neŭtrala tono: sen supersigno
zh_pr_silab(Paroj,5,SilabS):- pairs_values(Paroj,SilabS).
 
% la silabo finiĝas per cifero, kiu difinas la supersignon de la 
% unua vokalo, vd. https://de.wikipedia.org/wiki/Pinyin
% KOREKTU: foje ne la unua vokalo ricevas la supersignon,
% ekz-e  xià jiā, regulo:
% ĉe pli ol unu vokalo: signo super unua vokalo a,e,o; la dua vokalo alikaze
zh_pr_silab(Paroj,D,SilabS) :-
    % ni havas nur unu vokalon: ni metos supersignon tie
    % aŭ ni havas pli ol unu vokalon, sed la unua estas a,e,o
    (
        member(2-_,Paroj), member(1-V,Paroj), member(V,[a,e,o])
        ; \+ member(2-_,Paroj)
    ),!,
    select(1-V,Paroj,1-VS,ParojS),
    zh_pr_vokal_super(V,D,VS),
    pairs_values(ParojS,SilabS).

zh_pr_silab(Paroj,D,SilabS) :- 
    % ni havas pli ol unu vokalon kaj ĝi devias de a,e,o: ni metos supersignon sur la duan
    select(2-V,Paroj,2-VS,ParojS),
    zh_pr_vokal_super(V,D,VS),
    pairs_values(ParojS,SilabS).
    
% metu supersignon super vokalo laŭ la cifero D (tono)   
zh_pr_vokal_super(V,D,VS) :-
    once((        
        member(D-V-VS,[
            1-a-'ā', 1-e-'ē', 1-i-'ī', 1-o-'ō', 1-u-'ū',1-u-'ǖ',
            2-a-'á', 2-e-'é', 2-i-'í', 2-o-'ó', 2-u-'ú',2-u-'ǘ',
            3-a-'ǎ', 3-e-'ě', 3-i-'ǐ', 3-o-'ǒ', 3-u-'ǔ',3-ü-'ǚ',
            4-a-'à', 4-e-'è', 4-i-'ì', 4-o-'ò', 4-u-'ù',4,-u-'ǜ'
        ])
        ;
        V = VS % se ne estas vokalo, eble estas eraro, sed ni silente kopias
    )).


% ni devas identigi la unuan kaj duan vokalon de silabo
% por apliki la regulojn, do ni transformas la silabon al paroj
% 0-x, 1-i, 2-a,... kie 1 kaj 2 montras la unuan kaj duan vokalon
% kaj 0 konsonantojn kaj pliajn vokalojn
zh_pr_vokal([],_,[]).
zh_pr_vokal([Lit|Rest],N,[N1-Lit|Paroj]) :-
    vokalo(Lit), N<2, !,
    N1 is N+1,
    zh_pr_vokal(Rest,N1,Paroj).

zh_pr_vokal([Lit|Rest],N,[0-Lit|Paroj]) :-
    zh_pr_vokal(Rest,N,Paroj).    

vokalo(Lit) :- member(Lit,[a,e,i,o,u,ü]).




