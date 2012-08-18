Nonterminals
command
command_list
command_list1
column_def
data_type
select_cmd
table_constraint_clause
table_contents_source
table_def_cmd
table_element
table_element_list
table_element_LIST1
table_scope
default_clause_OPT default_clause
column_constraint_def_OPT column_constraint_def

.

Terminals
 id string number

 %---------- DDL --------------------
 % Verbs
 'CREATE' 'ALTER' 'DROP'

 % Nouns
 'DATABASE' 'TABLE' 'INDEX'

 % Other
 'DEFAULT' 'CONSTRAINT'

 %---------- DML --------------------
 % Verbs
 'INSERT' 'SELECT' 'UPDATE' 'DELETE'

 % Connecting words
 'INTO' 'AS' 'BY'

 % Query clauses
 'FROM' 'WHERE' 'JOIN' 'ORDER' 'GROUP'

 % Join types
 'LEFT' 'RIGHT' 'INNER' 'CROSS'

 % Boolean operators
 'AND' 'OR' 'NOT'

 % Set operators
 'IN' 'ALL' 'DISTINCT'

 % Null-related
 'IS' 'NULL'

 % Arithmetics
 '+' '-' '*' '/'

 % Comparisons
 '<' '=' '>' '<=' '>=' '<>'

 % Text operators
 'LIKE' '||'

 % Other delimiters
 '(' ')' ',' '.' ';' ':'.

Rootsymbol command_list.

command_list -> '$empty'.
command_list -> command.
command_list -> command ';' command_list.

command -> select_cmd.
command -> table_def_cmd.

%%---------- Create table
table_def_cmd -> 'CREATE' table_scope 'TABLE' id table_contents_source.

table_scope -> '$empty'.

table_contents_source -> table_element_list.

table_element_list -> '(' table_element_LIST1 ')'.

table_element -> column_def.
table_element -> table_constraint_clause.

table_constraint_clause -> '$empty'.

column_def -> id data_type default_clause_OPT column_constraint_def_OPT.

default_clause -> 'DEFAULT'. %TODO
column_constraint_def -> 'CONSTRAINT'. % TODO

%%---------- Select
select_cmd -> 'SELECT' id 'FROM' id ';'
        : [select, pos('$1'), '$2', '$4'].

%%---------- Types
data_type -> id.


%%---------- OPT/LIST/LIST1

table_element_LIST1 -> table_element.
table_element_LIST1 -> table_element ',' table_element_LIST1.


default_clause_OPT -> default_clause.
default_clause_OPT -> '$empty'.

column_constraint_def_OPT -> column_constraint_def.
column_constraint_def_OPT -> '$empty'.

Erlang code.

pos({_,Pos,_}) -> Pos.