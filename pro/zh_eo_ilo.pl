% (c) 2023 Wolfram Diestel
% laŭ GPL 2.0
%
% helpas trovi ĉinajn tradukojn per manklisto eo_de kaj publika vortaro han_de (handedict)
% komparante ambaŭ listojn. Akceptitaj proponoj iras al cellisto, kiun ni poste povos ŝovi en la 
% vortaron per merge_trd_xml.pl

% FARENDA:
% traduko de prononcoj al supersignaj vokaloj
% adicio de zh-tradukoj, se ili validas por diversaj de-tradukoj de la sama senco

% uzo:
% ricevi tradukproponojn por diversaj sencoj (markoj) de abidiki:
%   p(abdiki).
% memori la proponojn 1-1, 1-2 kaj 2-2
%  s(1-1), s(1-2), s(2-2).

:- use_module(library(csv)).
:- use_module(library(persistency)).
:- use_module(library(isub)).

:- dynamic manko/6, zhde/2, celo/3, propono/5.
:- persistent celo(eo:atom, mrk: atom, zh: atom).

:- initialization((
    writeln('Enlegu vortarojn per ''legu.'' antaŭ serĉi iujn proponojn per ''proponoj_eo(Vorto,Max).''!'),nl
    )).


/* ricevi vortojn sen trd 'zh' kun evtl. trd 'de'

  SELECT r3mrk.mrk, r3kap.kap, r3mrk.num, r3ofc.dos, de.ind, de.trd FROM r3mrk 
  LEFT JOIN r3trd AS zh ON (zh.mrk = r3mrk.mrk or zh.mrk = r3mrk.drv) AND zh.lng = 'zh' 
  LEFT JOIN r3trd AS de ON (de.mrk = r3mrk.mrk or de.mrk = r3mrk.drv) AND de.lng = 'de' 
  LEFT JOIN r3kap ON r3kap.mrk = r3mrk.drv 
  LEFT JOIN r3ofc ON r3kap.mrk = r3ofc.mrk
  WHERE r3mrk.mrk like 'a%' AND (ele='snc' OR ele='drv') AND zh.mrk IS NULL 
    AND (de.ekz = '' OR de.ekz is null)
  ORDER BY r3mrk.mrk LIMIT 200;
*/


/* HanDeDict
 vd http://www.handedict.de/chinesisch_deutsch.php?mode=dl
 aŭ pli nova: https://handedict.zydeo.net/de/download
 
 traduki la prononcindikojn de handedict, ekzemploj
 bei3 jing1 -> běi jīng
 kao3 ya1 ->  kǎo yā
*/

%csv_hande('vrt/handedict_nb.u8').
csv_hande('vrt/handedict_23.u8').

% ŝanĝu sufikson _a al alia litero kiam vi prilaboras aliajn literojn!
csv_mankoj('vrt/eo_de_d.csv').
db_celo('pdb/eo_zh_d.db').
csv_celo('vrt/eo_zh_d.csv').


legu :-
    db_celo(DB),
    db_attach(DB, []),   

    % csv_celo(C), format('legante ~w...~n',C),
    % legu_csv(celo, C, [separator(0';),skip_header('#')]),

    csv_mankoj(M), format('legante ~w...~n',M),
    legu_csv(manko, M, [separator(9),skip_header('#')]), % anst. tAB vi povas uzi ekz-e punktokomon: separator(0';)

    csv_hande(H), format('legante ~w...~n',H),
    legu_csv(hande, H, [separator(0'/),ignore_quotes(true),convert(false),skip_header('#'),match_arity(false)]),
    hande_redukt.

% skribas kolektiajn en celo tradukojn al CSV-dosiero
skribu :-
    csv_celo(Cel),
    setup_call_cleanup(
        open(Cel, write, Out),
        forall(
            celo(Eo,Mrk,Zh),
            csv_write_stream(Out,[row(Eo,Mrk,Zh)],[separator(0';)])
        ),
        close(Out)).    

skr_pr(Pr,Zh,ZhPr) :-
    normalize_space(atom(ZhN),Zh),
    normalize_space(atom(PrN),Pr),
    atomic_list_concat([ZhN,PrN],ZhPr).

% legas CSV-dosieron kaj kreas faktojn    
legu_csv(Pred,Dos,Opt) :-    
    csv_read_file(Dos, Dat, Opt),
    forall(
        member(R, Dat),
        (
            R =.. [_|L],
            Fakto =.. [Pred|L],
            assertz(Fakto)
        )
    ).

% handedict havas inter 3 kaj 22 kampojn - lasta kutime malplena
% ni transformas tion al duopoj
hande_redukt :-
    forall((
        between(3,22,A),
        functor(F,hande,A),
        catch(F,_,true),
        F =.. [_,Zh|De],
        member(D,De),
        D \= ''
        ),
        assertz(zhde(Zh,D))
    ).

% Ni iom simpligas la dialogon 
% permesante doni komencan demandsignon kaj serĉvorton,
% do ?<serĉvorto>. anstataŭ p(<serĉvorto>).
%expand_query(Query,Query,Bind,Bind) :- 
%    format('~w ~w~n',[Query,Bind]),
%    sub_atom(Query,0,1,_,'?'),
%    sub_atom(Query,1,_,0,QV),
%    normalize_space(QV,Vorto),
%    format('~w ~w ~w~n',[Query,Vorto,Bind]).

:- op(800,fy,user:(?)).
?(Eo) :- p(Eo).
?(Eo-De) :- pde(Eo,De).

% per kajsigno ni aldonas tradukojn per siaj numberoj
:- op(1200,fy,user:(&)).
&(Kion) :- s(Kion).
a :- s(a1).
b :- s(b1).
ab :- s(ab1).
abc :- s(abc1).

% informu pri kelkaj mankantaj tradukvortoj (eo)
% dum mankas ĉiuj, pli facile estas verŝajne simple trairi supre malsupren la
% CSV-dosieron per ordinara redaktilo / rigardilo.

m(Prefix) :- forall(
    limit(30,(
        manko(Eo,_,_,_,_),
        sub_atom(Eo,0,_,_,Prefix),
        mg(Eo,_))
    ),
    true).

% elektu iun vorton, por kiu ankoraŭ mankas traduko ĉina
% kaj informe skribu sencojn
m(Eo,Mrk,Ofc) :-
    manko(Eo,Mrk,_,Ofc,_),
    \+ celo(Eo,Mrk,_),
    format('~w;~w;~w~n',[Eo,Mrk,Ofc]).

% simile, sed grupigu ĉiujn germanajn tradukojn de laŭ marko
mg(Eo,Mrk) :-
    manko_grup(Eo,Mrk,Tradukita,LOfc,LDe),
    % germanaj tradukoj - listo por sama mrk
    atomic_list_concat(LDe,',',SDe),
    atomic_list_concat(LOfc,',',SOfc),
    trad_stat(Tradukita,TStat),
    format('~w~w: ~w;~w;~w~n',[TStat,Eo,Mrk,SOfc,SDe]).

% serĉu po 20 proponojn por la unuopaj germanj tradukoj de esperanta vorto
p(Eo) :- proponoj_eo(Eo,20).
% se ni scias, ke por devia germana traduko ni trovos ĉinan tradukon...
pde(Eo,De) :-
    retractall(propono(1,_,_,_,_)),
    proponoj_de(De,20,1,Eo,'').

% memoru proponon <Na><N> por posta skribo al csv_celo
% eblas ĉenigi la Na por registri plurajn: s(abgt1).
% cetere oni povas implice forlasi la 1
s(Kion) :- 
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
        s(N,L)
    ).

s(N,Na) :-
    propono(N,Na,Eo,Mrk,Zh),
    zh_prononco(Zh,ZhPr),
    once((
        celo(Eo,Mrk,ZhPr),
        format('jam ekzistas: ~w;~w;~w~n',[Eo,Mrk,ZhPr])
        ;
        assert_celo(Eo,Mrk,ZhPr),
        format('aldonita: ~w;~w;~w~n',[Eo,Mrk,ZhPr])
    )),!.

% forigu evtl. antaŭe memoratajn pri tiu senco
% antaŭ aldoni novan
sf(N,N1) :-
    propono(N,N1,Eo,Mrk,Zh),
    zh_prononco(Zh,ZhPr),
    retractall_celo(Eo,Mrk,_),
    assert_celo(Eo,Mrk,ZhPr),
    format('aldonita: ~w;~w;~w~n',[Eo,Mrk,ZhPr]).

% registru proponon sub certa (alia) esperanto-vorto
seo(Eo,Mrk,Na,N) :-
    propono(N,Na,_,_,Zh),
    zh_prononco(Zh,ZhPr),
    once((
        celo(Eo,Mrk,ZhPr),
        format('jam ekzistas: ~w;~w;~w~n',[Eo,Mrk,ZhPr])
        ;
        assert_celo(Eo,Mrk,ZhPr),
        format('aldonita: ~w;~w;~w~n',[Eo,Mrk,ZhPr])
    )),!.


proponoj_eo(Eo,Max) :-
    call_nth(manko_grup(Eo,Mrk,Tradukita,LOfc,LDe),N),
    retractall(propono(N,_,_,_,_)),
    atomic_list_concat(LDe,', ',SDe),
    atomic_list_concat(LOfc,', ',SOfc),    
    trad_stat(Tradukita,TStat),    
    format('x~d~w ~w [~w] (~w) ~w~n',[N,TStat,Eo,Mrk,SOfc,SDe]),
    proponoj_de(SDe,Max,N,Eo,Mrk).

proponoj_de(SDe,Max,N,Eo,Mrk) :-
    MaxS is Max*100, % isub estas rapida kaj donas plej bonajn ŝancojn serĉi mallongajn vortojn inter multaj
    forall(
        call_nth(limit(Max, % post ordigo ni limigas al kiom ni volas
            order_by([desc(Simil)], 
                    distinct(Zh,
                        (
                            member(Mtd-Mx,[e-Max,s-MaxS,n-Max]),
                            limit(Mx,de_zh(Mtd,SDe,De1,Zh,Simil))
                        )
                        %concurrent(3,
                        %    [
                        %        (Mtd = e, limit(Max2,de_zh(Mtd,SDe,De1,Zh,Simil))),
                        %        (Mtd = s, limit(Max2,de_zh(Mtd,SDe,De1,Zh,Simil))),
                        %        (Mtd = n, limit(Max2,de_zh(Mtd,SDe,De1,Zh,Simil)))
                        %    ],[])
                    )
                )
            ),
            N1), 
        (
            % N-N1 servas por poste identigi unuopan traduk-proponon, ni memoras ilin tiucele
            Nc is 0'a + N1 -1,
            char_code(Na,Nc),
            assertz(propono(N,Na,Eo,Mrk,Zh)),
            catch( % okazas iuj nevalidaj unikod-literoj !?
                format('(~w~1f) ~w~d: ~w, ~w~n',[Mtd,Simil,Na,N,De1,Zh]), 
                E,
                writeln(E))
        )
    ).
    
%proponoj_de(LstDe,Max,N,Eo,Mrk) :-
%    Max2 is Max * 2,
%    forall(
%        call_nth(
%            order_by([desc(Poentoj)], limit(Max,(
%                % serĉu ĉinajn tradukojn por ĉiuj unuopaj germanaj tradukoj
%                % kaj grupigu laŭ la ĉian traduko
%                group_by(Zh,Simil-De1,(
%                    member(De,LstDe),
%                    limit(Max2,de_zh(_,De,De1,Zh,Simil))
%                ), Rezultoj),
%                % ni nun adicias la similecojn al poentoj, se ĉina traduko troviĝas plurfoje
%                foldl(trdkunigo,Rezultoj,0-'',Poentoj-DeTrdj)
%        ))),
%            N1), 
%        (
%            % N-N1 servas por poste identigi unuopan traduk-proponon, ni memoras ilin tiucele
%            assertz(propono(N,N1,Eo,Mrk,Zh)),
%            format('~d-~d (~1f): ~w, ~w~n',[N,N1,Poentoj,DeTrdj,Zh])
%        )
%    ).

trdkunigo(P1-T1,P2-T2,P-T) :- 
    P is P1+P2, 
    once((
        T2 = '', T=T1
        ;
        atomic_list_concat([T1,T2],',',T)
    )). 

manko_grup(Eo,Mrk,Tradukita,OfcLst,DeLst) :-
    distinct(Mrk,manko(Eo,Mrk,_,_,_)),
    once((
        celo(Eo,Mrk,_), Tradukita = true % celtraduko jam registrita
        ;
        Tradukita = false % ankoraŭ mankas traduko zh
    )),    
    % germanaj tradukoj - listo por sama mrk
    group_by(Mrk,De,
        distinct(De,manko(Eo,Mrk,_,_,De)),
        DeLst),
    % oficialeecoj - listo por sama mrk
    group_by(Mrk,Ofc,
        distinct(Ofc,manko(Eo,Mrk,_,Ofc,_)),
        LOfc),
    exclude(=('NULL'),LOfc,OfcLst).

trad_stat(Tradukita,TStat) :- member(Tradukita-TStat,[true-'+',false-'-']).


manko(Eo,Mrk,No,Ofc,De) :-
    manko(Mrk,Eo,No,Ofc,DeI,DeT),
    once((
        DeT \= 'NULL', DeT \= '', De = DeT
        ;
        DeI \= 'NULL', DeI \= '', De = DeI
    )).

% provu trovi parojn eo - zh uzante ambaŭ vortarojn eo-de kaj zh-de
de_zh(e,De,De,Zh,1.0) :- 
        zhde(Zh,De).
        
de_zh(n,De,D1,Zh,Simil) :- 
        zhde(Zh,D1), 
        kmp_ngram(De,D1,Simil).

de_zh(s,De,D1,Zh,Simil) :- 
        zhde(Zh,D1), 
        kmp_isub(De,D1,Simil).

kmp_isub(T1,T2,Simil) :-
    %catch( % okazas iuj nevalidaj unikod-literoj !?
    %    (
            downcase_atom(T1,S1),
            downcase_atom(T2,S2),
            isub(S1,S2,Simil,[zero_to_one(true)])
    %    ), 
    %    _E,
    %    (
    %        %writeln(E),
    %        zhde(Zh,T2),
    %        format('uk: ~w~n',[Zh]),
    %        Simil = 0.0
    %    )
    %)
    ,
    Simil > 0.5 .

kmp_ngram(T1,T2,Simil) :-
    atom_length(T1,L), L>3,
    ngrams(T1,4,NGrams),
    kmp_ngram_(NGrams,T2,Simil),
    Simil > 0.4. %0.3 .

kmp_ngram_(NGrams,Atom,Percentage) :-
    atom_length(Atom,L), L>3, Len is L-3,
    % proper_length(NGrams,Len), Len>0,
    ngram_count(NGrams,Atom,0,Count),
    Percentage is Count / Len * 1.0. % ni multiplikas per 1.0 por eviti entjeron "1"

ngram_count([],_,C,C).
ngram_count([NGram|More],Atom,C,Count) :-
    once((
	sub_atom(Atom,_,_,_,NGram),
	C1 is C+1
	;
	C1=C
    )),
    ngram_count(More,Atom,C1,Count).
  
%! ngrams(+Atom,+Len,-NGramoj) is det
%
% Dishakas =Atom= en n-gramojn kun la longo =N=.
ngrams(Atom,Len,NGrams) :- 
  setof(NGram,A^B^sub_atom(Atom,B,Len,A,NGram),NGrams).    

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
    append(Silab,[C],SD), % silabo konsistas el askiaj literoj kaj fina cifero
    zh_pr_vokal(Silab,0,Paroj),
    atom_number(C,D),
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