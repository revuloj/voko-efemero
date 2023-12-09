% (c) 2023 Wolfram Diestel
% laŭ GPL 2.0
%
% helpas trovi ĉinajn tradukojn per manklisto eo_de kaj publika vortaro han_de (handedict)
% komparante ambaŭ listojn. Akceptitaj proponoj iras al cellisto, kiun ni poste povos ŝovi en la 
% vortaron per merge_trd_xml.pl

% uzo:
% ricevi tradukproponojn por diversaj sencoj (markoj) de abidiki:
%   p(abdiki).
% memori la proponojn 1-1, 1-2 kaj 2-2
%  s(1-1), s(1-2), s(2-2).


/* ricevi vortojn sen trd 'zh' kun evtl. trd 'de'
  SELECT r3kap.kap, r3mrk.mrk, trd.lng, trd.trd, trd.ind 
  FROM r3mrk 
  LEFT JOIN r3trd AS zh ON (zh.mrk = r3mrk.mrk or zh.mrk = r3mrk.drv) AND zh.lng = 'zh' 
  LEFT JOIN r3trd AS trd ON (trd.mrk = r3mrk.mrk or trd.mrk = r3mrk.drv) AND trd.lng in ('de','en','fr') 
  LEFT JOIN r3kap ON r3kap.mrk = r3mrk.drv 
  WHERE r3mrk.mrk like 'h%' AND (ele='snc' OR ele='drv') AND zh.mrk IS NULL 
    AND (trd.ekz = '' OR trd.ekz is null)
  ORDER BY r3mrk.mrk LIMIT 200;
*/

:- use_module(library(csv)).
:- use_module(library(isub)).
:- use_module(library(persistency)).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).

:- multifile http:location/3.
:- dynamic eo/5, trd/3, trd/4, propono/5, celo/3, http:location/3.
:- persistent celo(eo:atom, mrk: atom, zh: atom).

user:file_search_path('ret','./ret').
http:location(sercho,root(sercho),[]).

:- initialization((
    writeln('Enlegu vortarojn per ''legu.'' aŭ lanĉu servon per start(Port).''!'),nl
    )).
%:- initialization(start_server(3333)). 

:- http_handler(root(sercho), sercho, []). 
:- http_handler(root(.), http_reply_from_files(ret(.),[]),[prefix]).

start(Port) :-
    legu,
    format('lanĉante serĉilon ĉe http://0.0.0.0:~w~n',Port),
    http_server(http_dispatch, [port(Port)]).

sercho(Request) :-
    debug(http(sercho),'Serĉo ~q',[Request]),
    http_read_json(Request, json(JSON)),
    %format('Content-type: text/plain~n~n'),
    member(q=Sercho,JSON),
    with_output_to(atom(Trovoj),zh_proponoj(eo,Sercho,20),[]),
    reply_json(json([trovoj=Trovoj])).
    
/* 
 zh-de: HanDeDict
 vd http://www.handedict.de/chinesisch_deutsch.php?mode=dl
 aŭ pli nova: https://handedict.zydeo.net/de/download
 
 zh-fr: CFDict:
 https://chine.in/mandarin/dictionnaire/CFDICT/

 zh-en: CEDict:
 https://www.mdbg.net/chinese/dictionary?page=cedict
 
 traduki la prononcindikojn de handedict, ekzemploj
 bei3 jing1 -> běi jīng
 kao3 ya1 ->  kǎo yā
*/

% ŝanĝu sufikson _a al alia litero kiam vi prilaboras aliajn literojn!
eo_def('vrt/eo_def_k.csv'). % eo-de/en/fr
db_celo('pdb/eo_zh_k.db').
csv_celo('vrt/eo_zh_k.csv').

/**
 * Dialogo por aldono de tradukoj:
 * a..t - elektu/konservu proponon kun tiu numero
 * RET, spaco - sekva propono por la aktuala vorto
 * x - finu
 * 
 * ALDONENAJ:
 * 1..2 - iru al senco/marko kun tiu cifero
 * ?, - iru al venonta vorto
 * % ^ - iru al la antaŭa propono
 */
dialog :- sekva(Eo), dialog(Eo).
dialog(Eo) :-
    % superrigardo pri proponoj
    format('## superrigardo:~n'),
    zh_proponoj(eo,Eo,-16), nl,
    % iteraciu tra proponoj por unuopajn konservi
    repeat, (
        zh_propono(eo,Eo,N,Na,20),
        format('-------------------------------~n'),
        format(' ⏎ konservu, ␠ sekva, x fino, a..t konservu laŭ litero >:'),
        get_single_char(K),
        format('<~w>~n',K),
        (
            K = 0'x, % finu
            true
            ;
            once((
                % ENTER: sekurigu la lastan (aktualan) proponon
                memberchk(K,[13]),
                atom_concat(Na,N,P),
                sekurigo(P)
                ;
                % sekurigu proponon laŭ donita litero
                between(0'a,0't,K),
                char_code(Ch,K),
                atom_concat(Ch,N,P),
                sekurigo(P)
                ;
                % montru sekvan proponon, klavoj SPAC, SUBEN
                memberchk(K,[32,27])
                ;
                % iru al marko/serio N
                % between(0'1,0'9,K),
                % number_codes(N,[K])
                %;
                writeln('❗nevalida klavo!')
            )),
            fail
        )
        ; % finu se ne troviĝas proponoj plu
        format('❗plu neniu propono~n'), true
    ),!.
 

legu :-
    % db por tradukoj eo-zh
    db_celo(DB),
    db_attach(DB, []),   

    % vortoj el Revo eo kun tradukoj de/en/fr
    eo_def(EDEF),
    legu_eo(EDEF),
    kvaropoj(eo),
 
    % ĉinaj vortaroj
    legu_zh(de,'vrt/handedict_23.u8'),
    legu_zh(en,'vrt/cedict.u8'),
    legu_zh(fr,'vrt/cfdict.u8'),
    triopoj(zh), !.

% Ni iom simpligas la dialogon 
% permesante doni komencan demandsignon kaj serĉvorton,
% do ?enhavo. anstataŭ zh_tradukoj(enhavo,10).
:- op(800,fy,user:(?)).
?(Eo) :- zh_proponoj(eo,Eo,20).
% mankas skribado ktp:
% ?(Eo-De) :- trd(zh-de,Zh,De).

% per kajsigno ni aldonas tradukojn per siaj numeroj/literoj
:- op(1200,fy,user:(&)).
&(Kion) :- sekurigo(Kion).
a :- sekurigo(a1).
b :- sekurigo(b1).
c :- sekurigo(c1).
ab :- sekurigo(ab1).
ac :- sekurigo(ac1).
bc :- sekurigo(bc1).
abc :- sekurigo(abc1).


/**
 * Enlegi la tradukojn de esperantaj vortoj el CSV-dosiero
 * poste ili estas pridemandeblaj per: 
 * ?- eo(Eo,Mrk,Lng,Trd,Ind).separator(0'/),ignore_quotes(true),convert(false),skip_header('#'),match_arity(false)
 */ 
legu_eo(EoCsv) :- 
    format('legante ~w...~n',EoCsv),
    csv_read_file(EoCsv, Datenopoj, [
        separator(9),     % uzu punktokomon kiel apartigilo de kampoj
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

kvaropoj(eo) :-
    forall(
        (
            eo(Eo,Mrk,Lng,Trd,Ind),
            member(Lng,[de,en,fr]) % Lng \= 'NULL'
        ),
        (
        Trd \= '' -> assertz(trd(eo-Lng,Eo,Mrk,Trd)) % - se Trd ne malplena -
        ; assertz(trd(eo-Lng,Eo,Mrk,Ind))
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
 *  skribas kolektiajn en celo tradukojn al CSV-dosiero
 */

skribu :-
    csv_celo(Cel),
    setup_call_cleanup(
        open(Cel, write, Out),
        forall(
            celo(Eo,Mrk,Zh),
            csv_write_stream(Out,[row(Eo,Mrk,Zh)],[separator(0';)])
        ),
        close(Out)).  

/**
 * eo_zh(ekzemplo,Zh,Ponto).
 */
eo_zh(Eo,Mrk,Zh,1.0-Lp:Ponto) :-
    member(Lp,[en,de,fr]),
    trd(eo-Lp,Eo,Mrk,Ponto), % ni povas trovi tradukon per pontlingvo
    trd(zh-Lp,Zh,Ponto).

eo_zh(Eo,Mrk,Zh,Simileco-Lp:P2) :-
    member(Lp,[en,de,fr]),
    limit(100,(
        trd(eo-Lp,Eo,Mrk,P1), % ni povas trovi tradukon per pontlingvo kaj simileco
        trd(zh-Lp,Zh,P2),
        P1 \= P2, % evitu duoble trovi/kalkuli tradukojn
        isub(P1,P2,Simileco,[zero_to_one(true)]),
        Simileco>0.7
    )).

eo_zh_grup(Eo,Mrk,Zh,SimSum-Pontoj) :-
    % serĉu parojn Eo-Zh 
    % kaj identajn paroj sumiĝu
    group_by(Eo-Mrk-Zh,
        Simil-Lp:Ponto,
        eo_zh(Eo,Mrk,Zh,Simil-Lp:Ponto),
        Trovoj),
    % ni nun adicias la similecojn al poentoj, por samaj paroj
    foldl(kunigo,Trovoj,0-[],SimSum-Pontoj).

kunigo(
    Sim1-Lp1:P1,
    Sim2-PList,
    SimSum-[Lp1:P1|PList]) :- 
        SimSum is Sim1+Sim2.

eo_zh_ord(Eo,Mrk,Zh,SimSum-Pontoj) :-
    order_by([desc(SimSum)], 
        eo_zh_grup(Eo,Mrk,Zh,SimSum-Pontoj)
    ).

zh_pontoj(Eo,Mrk,Kiom) :-
    forall(
    limit(Kiom,eo_zh_ord(Eo,Mrk,Zh,SimSum-Pontoj)),
    format('~1f ~w ~36|~w~n',[SimSum,Zh,Pontoj])
    ).

zh_trdj(Zh,Tradukoj) :-
    setof(Lng:Trd,trd(zh-Lng,Zh,Trd),Tradukoj).

zh_tradukoj(Eo,Mrk,Kiom) :-
    forall(
        limit(Kiom,eo_zh_ord(Eo,Mrk,Zh,SimSum-_)),
        (
            zh_trdj(Zh,Tradukoj),
            format('~1f ~w ~36|~w~n',[SimSum,Zh,Tradukoj])
        )
    ).

eo_mrk(Eo,Mrk) :-
    distinct(Mrk,eo(Eo,Mrk,_,_,_)).

mrk_trdj(Mrk,Tradukoj) :-
    setof(Lng:Trd,Kap^trd(eo-Lng,Kap,Mrk,Trd),Tradukoj).

tradukita(Mrk,TStat) :-
    once((
        celo(_,Mrk,_), TStat = '+' % celtraduko jam registrita
        ;
        TStat = '-' % ankoraŭ mankas traduko zh
    )).  

skribu_trd(Tradukoj) :-
    forall(
        group_by(Lng, Trd, member(Lng:Trd,Tradukoj), LngTrd),
        (
            atomic_list_concat(LngTrd,'; ',TrdStr),
            %format('          ~w: ~w~n',[Lng,TrdStr])
            format('~7|~w: ',Lng), skribu_lin(TrdStr,10,70)
        )
    ).    

skribu_lin(Str,Indent,LLen) :-
    % mallongajn Str skribu sur unu linio
    atom_length(Str,Len), Len < LLen, !,
    format('~*|~w~n',[Indent,Str]).

skribu_lin(Str,Indent,LLen) :-
    % longajn Str: skribu kiom eblas de la komenco sur linio
    % rompante nur ĉe spaco
    findall(A,sub_atom(Str,A,1,_,' '),Spacoj),
    reverse(Spacoj,RSpac),
    once((
        member(S,RSpac), S < LLen
        ;
        S = LLen
    )),
    sub_atom(Str,0,S,_,Komenco),
    format('~*|~w~n',[Indent,Komenco]),
    sub_atom(Str,S,_,0,Resto),
    skribu_lin(Resto,Indent,LLen).


/**
 * Trovas unu post alia proponojn por traduki vorton Eo/Mrk al ĉina kaj skribas la proponojn ankaŭ
 * N,Na servas por identigi unuopan proponon (faktoj propono(N,Na,Eo,Mrk,Zh))
 * Kiom donas maksimuman nombron por porponoj, se negativa ni pli koncize skribas la proponojn
 * kaj ne savas ilin en faktoj propono/5.
 * 
 */ 
zh_propono(eo,Eo,Mrk,N,Na,Kiom) :-
    retractall(propono(N,_,_,_,_)),
    Max is abs(Kiom),
    call_nth(
        limit(Max,eo_zh_ord(Eo,Mrk,Zh,SimSum-_)),
        N1),
    Nc is 0'a + N1 -1,
    char_code(Na,Nc),
    once((
        Kiom>0,
        assertz(propono(N,Na,Eo,Mrk,Zh)),
        zh_trdj(Zh,Tradukoj),
        format('~n-------------------------------~n'),
        format('(~1f) [~w~d]: ~w~n',[SimSum,Na,N,Zh]),
        skribu_trd(Tradukoj)
        ;
        zh_trdj(Zh,Tradukoj),
        format('✩ (~1f) [~w~d]: ~w ~w~n',[SimSum,Na,N,Tradukoj,Zh])
    )).

    

%zh_proponoj(eo,Eo,Mrk,N,Kiom) :-
%    forall(
%        call_nth(
%            limit(Kiom,eo_zh_ord(Eo,Mrk,Zh,SimSum-_)),
%            N1),
%        (
%            Nc is 0'a + N1 -1,
%            char_code(Na,Nc),
%            assertz(propono(N,Na,Eo,Mrk,Zh)),
%            zh_trdj(Zh,Tradukoj),
%            format('(~1f) [~w~d]: ~w~n',[SimSum,Na,N,Zh]),
%            skribu_trd(Tradukoj)
%        )
%    ).

/**
 * Trovas unu post alia proponojn por traduki vorton Eo al ĉina kaj skribas la proponojn ankaŭ
 * N,Na servas por identigi unuopan proponon (faktoj propono(N,Na,Eo,Mrk,Zh))
 * Kiom donas maksimuman nombron por porponoj, se negativa ni pli koncize skribas la proponojn
 * 
 */ 
zh_propono(eo,Eo,N,Na,Kiom) :-
    retractall(propono(_,_,_,_,_)),
    % kiuj diversaj Markoj/Sencoj troviĝas por vorto Eo?
    findall(N-Mrk,call_nth(eo_mrk(Eo,Mrk),N),Sencoj),
    % ni unue listigas ĉiujn ĉi
    forall(
        member(N-Mrk,Sencoj),
        (
            tradukita(Mrk,TStat),    
            format('~d~w: ~w ~w~n',[N,TStat,Eo,Mrk]),
            (mrk_trdj(Mrk,Trdj);Trdj=[]),
            skribu_trd(Trdj)
        )
    ),
    % nun ni serĉu kaj prezentu tradukproponojn por ili
    member(N-Mrk,Sencoj),
    format('~n### ❓~w~n',Mrk),
    zh_propono(eo,Eo,Mrk,N,Na,Kiom).

zh_proponoj(eo,Eo,Mrk,N,Kiom) :-
    forall(zh_propono(eo,Eo,Mrk,N,_,Kiom),true).

zh_proponoj(eo,Eo,Kiom) :-
    % kiuj diversaj Markoj/Sencoj troviĝas por vorto Eo?
    findall(N-Mrk,call_nth(eo_mrk(Eo,Mrk),N),Sencoj),
    % ni unue listigas ĉiujn ĉi
    forall(
        member(N-Mrk,Sencoj),
        (
            tradukita(Mrk,TStat),    
            format('~d~w: ~w ~w~n',[N,TStat,Eo,Mrk])
        )
    ),
    % nun ni serĉu kaj prezentu tradukproponojn por ili
    forall(
        member(N-Mrk,Sencoj),
        (
            format('### ~w~n',Mrk),
            zh_proponoj(eo,Eo,Mrk,N,Kiom)
        )
    ).

/**
 * Konciza predikatoj por ricevi proponoj por Eo-vortoj
 * serĉu po 20 proponojn por (sekva en vico) esperanta vorto
 */
propono :- 
    sekva(Eo),
    zh_proponoj(eo,Eo,20).

propono(Eo) :- 
    once((
        nonvar(Eo)
        ;
        var(Eo),
        sekva(Eo)
    )),
    zh_proponoj(eo,Eo,20).

% se ni scias, ke por devia germana traduko ni trovos ĉinan tradukon...
% pde(Eo,De) :-
%     retractall(propono(1,_,_,_,_)),
%     proponoj_de(De,20,1,Eo,'').

% memoru proponon <Na><N> por posta skribo al csv_celo
% eblas ĉenigi la Na por registri plurajn: s(abgt1).
% cetere oni povas implice forlasi la 1
sekurigo(Kion) :- 
    atom_chars(Kion,Kodoj),
    once((
        append(Literoj,[Cifero],Kodoj),
        atom_number(Cifero,N),
        between(1,9,N)
        ;
        N = 1,
        Literoj = Kodoj
    )),
    forall(
        member(L,Literoj),
        sekurigo(N,L)
    ).

sekurigo(N,Na) :-
    propono(N,Na,Eo,Mrk,Zh),
    zh_prononco(Zh,ZhPr),
    once((
        celo(Eo,Mrk,ZhPr),
        format('❗jam ekzistas: ~w;~w;~w~n',[Eo,Mrk,ZhPr])
        ;
        assert_celo(Eo,Mrk,ZhPr),
        format('✅ aldonita: ~w;~w;~w~n',[Eo,Mrk,ZhPr])
    )),!.

% forigu evtl. antaŭe memoratajn pri tiu senco
% antaŭ aldoni novan
sekurigo_for(N,N1) :-
    propono(N,N1,Eo,Mrk,Zh),
    zh_prononco(Zh,ZhPr),
    retractall_celo(Eo,Mrk,_),
    assert_celo(Eo,Mrk,ZhPr),
    format('aldonita: ~w;~w;~w~n',[Eo,Mrk,ZhPr]).

% registru proponon sub certa (alia) esperanto-vorto
sekurigo_eo(Eo,Mrk,Na,N) :-
    propono(N,Na,_,_,Zh),
    zh_prononco(Zh,ZhPr),
    once((
        celo(Eo,Mrk,ZhPr),
        format('jam ekzistas: ~w;~w;~w~n',[Eo,Mrk,ZhPr])
        ;
        assert_celo(Eo,Mrk,ZhPr),
        format('aldonita: ~w;~w;~w~n',[Eo,Mrk,ZhPr])
    )),!.

trad_stat(Tradukita,TStat) :- member(Tradukita-TStat,[true-'+',false-'-']).

/**
 * Trovas la lastan konservitan tradukon eo-zh
 */
lasta(Eo) :-
    order_by([desc(N)],call_nth(celo(Eo,_,_),N)).


/**
 * Sekva en vico post iu Esperanto-vorto.
 * Voku komence por meti komencan numeron en variablo 'lasta'
 */ 
sekva(Eo,Sekva) :-
    call_nth(eo(Eo1,_,_,_,_),N), Eo=Eo1,
    nb_setval(lasta,N),
    sekva(Sekva).

/**
 * Sekva laŭ vico post 'lasta', ni transsaltas samajn kapvortoj/markojn
 */ 
sekva(Sekva) :-
    once((
        catch(nb_getval(lasta,Lasta),_,fail),
        call_nth(eo(Eo,Mrk,_,_,_),Lasta)
        ;
        lasta(Eo),
        call_nth(eo(EoL,Mrk,_,_,_),Lasta),
        Eo = EoL
        ;
        propono(_,_,Eo,Mrk,_),
        call_nth(eo(EoN,MrkN,_,_,_),Lasta),
        Eo=EoN, MrkN=Mrk
        ;
        Lasta=1, Eo='', Mrk=''
    )),
    % trovu la sekvan vorton en mankoj
    call_nth(eo(Sekva,MrkS,_,_,_),N1),
    N1>Lasta, Sekva\=Eo, MrkS\=Mrk, !,
    nb_setval(lasta,N1),
    format('..~d: ~w~n',[N1,Sekva]).


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
    % ni ne atendu tekston post la prononco, ĉu?
    % sub_atom(Zh,K2,_,0,Post),

    % forigu tradukojn duoblajn, metu komojn
    zh_listo(Ant,ZhPr,PrS),!. 

zh_listo(L,L1,Pr) :-
    atomic_list_concat(Tj,' ',L),
    % forigu evtl. duoblajn
    setof(T1,
        T^(
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
        member(2-_,Paroj), member(1-V,Paroj), member(V,[a,e,o,A,E,O])
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
            4-a-'à', 4-e-'è', 4-i-'ì', 4-o-'ò', 4-u-'ù',4-ü-'ǜ',
            1-A-'Ā', 1-E-'Ē', 1-I-'Ī', 1-O-'Ō', 1-U-'Ū',1-U-'Ǖ',
            2-A-'Á', 2-E-'É', 2-I-'Í', 2-O-'Ó', 2-U-'Ú',2-U-'Ǘ',
            3-A-'Ǎ', 3-E-'Ě', 3-I-'Ǐ', 3-O-'Ǒ', 3-U-'Ǔ',3-Ü-'Ǚ',
            4-A-'À', 4-E-'È', 4-I-'Ì', 4-O-'Ò', 4-U-'Ù',4-Ü-'Ǜ'
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

vokalo(Lit) :- member(Lit,[a,e,i,o,u,ü,A,E,I,O,U,Ü]).