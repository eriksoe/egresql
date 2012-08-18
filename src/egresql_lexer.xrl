Definitions.

L=[A-Za-z_]
D=[0-9]
WS=[\s\t\r\n\v]

Rules.
{L}+   : {token, identifier_or_keyword(TokenChars, TokenLine)}.
{L}({L}|{D})+   : {token, identifier(TokenChars, TokenLine)}.
%[Ss][Ee][Ll][Ee][Cc][Tt] : {token, {select, TokenLine, select}}.
{WS}+  : skip_token.
{D}+   : {token, {int, TokenLine, list_to_integer(TokenChars)}}.
% TODO: Floating-point numbers.

% Single-character operators:
([+-/<=>;()]|\*) : {token, {list_to_atom(TokenChars), TokenLine}}.

Erlang code.
keyword_list() ->
  ["SELECT","FROM","WHERE","JOIN","AS","ORDER","GROUP","BY",
  "LEFT","RIGHT","INNER","CROSS",
  "UPDATE","DELETE",
  "CREATE","DROP",
  "TABLE","INDEX",
  "AND", "OR", "IN"].

identifier_or_keyword(Chars, Line) ->
  UpperChars = string:to_upper(Chars),
  case lists:member(UpperChars, keyword_list()) of
    true ->
      Keyword = list_to_atom(UpperChars),
      {Keyword, Line};
    false ->
      identifier(Chars,Line)
  end.

identifier(Chars, Line) ->
  {id, Line, Chars}.
