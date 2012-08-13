Nonterminals command.

Terminals
 id string number
 'SELECT' 'FROM' 'WHERE' 'JOIN' 'AS' 'ORDER' 'GROUP' 'BY' 'INTO'
 'LEFT' 'RIGHT' 'INNER' 'CROSS'
 'UPDATE' 'DELETE'
 'CREATE' 'DROP' 'ALTER'
 'DATABASE' 'TABLE' 'INDEX'
 'AND' 'OR' 'IN' 'NOT'
 'IS' 'NULL'
 'ALL' 'DISTINCT'
 '+' '-' '*' '/' ';'.

Rootsymbol command.

command -> 'SELECT' id 'FROM' id ';'
        : [select, pos('$1'), '$2', '$4'].


Erlang code.

pos({_,Pos,_}) -> Pos.