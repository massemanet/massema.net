% mustasch lexer grammar.
% tokens are;
% '}}': end token
% '.', ':': as themselves
% em: escaped mustache ("\{", "\}")
% sq: single quoted string
% dq: double quoted string
% uq: unquoted text

Definitions.

UQ = [^'"}:\.]
CM = }
WS = ([\000-\s])

Rules.

{UQ}+ :
  {token,{uq,TokenLine,TokenChars}}.

"([^"]|\\")*" :
  {token,{dq,TokenLine,btrim(TokenChars)}}.

'([^']|\\')*' :
  {token,{sq,TokenLine,btrim(TokenChars)}}.

\. :
  {token,{'.',TokenLine}}.

: :
  {token,{':',TokenLine}}.

{WS}*{CM}{CM} :
  {end_token,{'}}',TokenLine}}.

Erlang code.

btrim(S) -> lists:reverse(tl(lists:reverse(tl(S)))).
