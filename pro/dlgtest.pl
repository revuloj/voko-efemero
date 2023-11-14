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

solution(P) :-
    member(P,[a,b,c,d,e,f,g,h,u,j]),
    format('solution ~w~n',P).

save(P) :-
    format('saving ~w...~n',P).