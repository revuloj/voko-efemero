:- use_module(library(csv)).

test:-
  csv_read_file('zhtest.csv',X,[separator(0'/),ignore_quotes(true),skip_header('#'),convert(false),match_arity(false),encoding(utf8)]),
  writeln(X).
