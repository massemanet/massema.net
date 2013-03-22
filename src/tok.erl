%% -*- mode: erlang; erlang-indent-level: 2 -*-
%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 22 Mar 2013 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('tok').
-author('mats cronqvist').
-export([tokenize/1]).

tokenize(Str) ->
  try lists:reverse(tok(wb,btrim(Str),[]))
  catch R -> R;
        _:_ -> erlang:get_stacktrace()
  end.

tok(dq,[]    ,_) -> throw({syntax_error,dq,eol});
tok(sq,[]    ,_) -> throw({syntax_error,sq,eol});
tok(uq,[$ |_],_) -> throw({syntax_error,illegal_whitespace});
tok(wb,[$ |_],_) -> throw({syntax_error,illegal_whitespace});
tok( _,[92]  ,_) -> throw({syntax_error,backslash,eol});
tok(St,[92|S],A) -> tok(St,tl(S),[[hd(S)|hd(A)]|tl(A)]);
tok(dq,[$"|S],A) -> tok(wb,S,[{dq,lists:reverse(hd(A))}|tl(A)]);
tok(dq,[Ch|S],A) -> tok(dq,S,[[Ch|hd(A)]|tl(A)]);
tok(sq,[$'|S],A) -> tok(wb,S,[{sq,lists:reverse(hd(A))}|tl(A)]);
tok(sq,[Ch|S],A) -> tok(sq,S,[[Ch|hd(A)]|tl(A)]);
tok(uq,[$.|S],A) -> tok(wb,S,[dot,{uq,lists:reverse(hd(A))}|tl(A)]);
tok(uq,[$:|S],A) -> tok(wb,S,[colon,{uq,lists:reverse(hd(A))}|tl(A)]);
tok(uq,[Ch|S],A) -> tok(uq,S,[[Ch|hd(A)]|tl(A)]);
tok(wb,[$"|S],A) -> tok(dq,S,[[]|A]);
tok(wb,[$'|S],A) -> tok(sq,S,[[]|A]);
tok(wb,[$.|S],A) -> tok(wb,S,[dot|A]);
tok(wb,[$:|S],A) -> tok(wb,S,[colon|A]);
tok(wb,[Ch|S],A) -> tok(uq,S,[[Ch]|A]);
tok(wb,[]    ,A) -> A;
tok(uq,[]    ,A) -> A.

btrim(Str) -> lists:reverse(ptrim(lists:reverse(ptrim(Str)))).

ptrim([$ |S]) -> ptrim(S);
ptrim(S) -> S.
