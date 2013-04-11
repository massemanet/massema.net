Nonterminals doc string nugget subnugget atom.
Terminals 'uq' 'dq' 'sq' 'em' '{{' '}}' '.' ':'.
Rootsymbol doc.

doc -> string                      : ['$1'].
doc -> doc '{{' nugget '}}' string : '$1' ++ ['$3'] ++ ['$5'].

string -> '$empty'    : [].
string -> string 'uq' : '$1' ++ val('$2').
string -> string 'dq' : '$1' ++ [$"] ++ val('$2') ++ [$"].
string -> string 'sq' : '$1' ++ [$'] ++ val('$2') ++ [$'].
string -> string 'em' : '$1' ++ val('$2').
string -> string '.'  : '$1' ++ [$.].
string -> string ':'  : '$1' ++ [$:].

nugget -> '$empty'             : [].
nugget -> subnugget            : ['$1'].
nugget -> nugget '.' subnugget : '$1' ++ ['$3'].

subnugget -> atom ':' atom : {'$1','$3'}.
subnugget -> atom          : '$1'.
subnugget -> 'dq'          : [$"] ++ val('$1') ++ [$"].

atom -> 'uq' : atom_or_int(val('$1')).
atom -> 'sq' : list_to_atom(val('$1')).

Erlang code.

val({_,_,V}) -> V.

atom_or_int(S) ->
  T = string:strip(S),
  try list_to_integer(T)
  catch _:badarg -> list_to_atom(T)
  end.
