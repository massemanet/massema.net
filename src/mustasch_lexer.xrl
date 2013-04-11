% mustasch lexer grammar.
% tokens are;
% '{{', '}}', '.', ':': as themselves
% em: escaped mustache ("\{", "\}")
% sq: single quoted string
% dq: double quoted string
% uq: unquoted text

Definitions.

UQ = [^'"{}:\.\\]

EM = (\\{|\\})

OM = {

CM = }

WS = ([\000-\s])

Rules.

({UQ}+|{OM}[^{OM}]|{CM}[^{CM}]|\\[^{OM}{CM}])+ :
  {token,{uq,TokenLine,TokenChars}}.

"([^"]|\\")*" :
  {token,{dq,TokenLine,btrim(TokenChars)}}.

'([^']|\\')*' :
  {token,{sq,TokenLine,btrim(TokenChars)}}.

\. :
  {token,{'.',TokenLine}}.

: :
  {token,{':',TokenLine}}.

{OM}{OM}{WS}* :
  {token,{'{{',TokenLine}}.

{WS}*{CM}{CM} :
  {token,{'}}',TokenLine}}.

{EM} :
  {token,{em,TokenLine,tl(TokenChars)}}.

Erlang code.

btrim(S) -> lists:reverse(tl(lists:reverse(tl(S)))).
