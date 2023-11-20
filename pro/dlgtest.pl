/**
 * Dialogi pri aldono de tradukoj:
 * p - iru al venonta vorto
 * e - elektu lastan proponon
 * n - sekva propono por la aktuala vorto
 * q - finu
 * r, ^ - iru al la antaŭa propono
 */
dialog :-
    repeat, (
        solution(P),
        get_single_char(K),
        format('key ~w~n',K),
        (
            K = 0'q,
            true
            ;
            once((
                % save solution
                K = 0'e,
                save(P)
                ;
                % sekurigu proponon laŭ donita litero
                between(0'a,0't,K), writeln(K)
                ;
                % got next solution
                memberchk(K,[0'n,32,13])
                ;
                writeln('invalid key')
            )),
            fail
        )
    ),!.

/**
 * Dialogi pri aldono de tradukoj, uzanten engines:
 * p - iru al venonta vorto
 * e - elektu lastan proponon
 * n - sekva propono por la aktuala vorto
 * q - finu
 * r, ^ - iru al la antaŭa propono
 */
dlge :-
    repeat, (
        engine_next(en,N),
        repeat, (
            engine_next(el,L),
            format('marko: ~w~n',N),
            format('traduko: ~w~n',L),
            get_single_char(K),
            format('key ~w~n',K),
            (
                % finu (exit)
                K = 0'x,
                true
                ;
                once((
                    % save solution
                    K = 13,
                    save(N,L)
                    ;
                    % sekurigu proponon laŭ donita litero
                    between(0'a,0't,K), writeln(K)
                    ;
                    % goto next solution
                    memberchk(K,[32])
                    ;
                    % goto next marko
                    memberchk(K,[0'1,0'2]),
                    engine_destroy(el)
                    ;
                    writeln('invalid key')
                )),
                fail
            ),
        fail)
    ),!.

engines :-
    engine_create(N,member(N,[1,2,3]),en),
    engine_create(L,member(L,[a,b,c,d,e,f,g]),el).

solution(P) :-
    member(P,[a,b,c,d,e,f,g,h,u,j]),
    format('solution ~w~n',P).

save(P) :-
    format('saving ~w...~n',P).


save(N,L) :-
    format('saving ~w.~w...~n',[N,L]).