Construct (Revision 2)
=============================

In this revision the data structures will be more loosely defined.

## Grammar
```
construct ::= ( object | named-object)*
object ::= symbol | number | string | block | list | ';'
named-object ::= (symbol | number | string) '=' object
block ::= '{' construct '}'
list ::= '(' (construct | ',')* ')'
```



