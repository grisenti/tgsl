# Grammar

## Program

```text
program → declaration* EOF ;
```

## Declarations

```text
declaration → struct_decl
	| fn_decl
	| var_decl
	| statement;

struct_decl → "struct" IDENTIFIER "{" (IDENTIFIER ":" IDENTIFIER ",")* "}";

fn_decl → "fn" IDENTIFIER "(" parameters? ")" -> IDENTIFIER block

parameters → IDENTIFIER ":" IDENTIFIER ( "," IDENTIFIER ":" IDENTIFIER)* ;

var_decl → "var" IDENTIFIER (":" IDENTIFIER)? ( "=" expression )? ";";
```

## Statements

```text
statement → expr_stmt
	| for_stmt
	| if_stmt
	| print_stmt
	| return_stmt
	| while_stmt
	| block ;

expr_stmt → expression ";" ;

for_stmt → "for" "(" IDENTIFIER in IDENTIFIER ")" statement ;

if_stmt → "if" "(" expression ")" statement ( "else" statement )? ;

return_stmt → "return" expression? ";" ;

while_stmt → "while" "(" expression ")" statement ;

block → "{" declaration* "}" ;
```

## Expressions

```text
expression → lambda | assignment;

lambda → "fn" "(" parameters? ")" -> IDENTIFIER block;

assignment → ( call "." )? IDENTIFIER "=" assignment
	| logic_or ;

logic_or → logic_and ( "or" logic_and )* ;

logic_and → equality ( "and" equality )* ;

equality → comparison ( ( "!=" | "==" ) comparison )* ;

comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;

term → factor ( ( "-" | "+" ) factor )* ;

factor → unary (( "/" | "*" ) unary)*;

unary → ( "!" | "-" ) unary | call ;

call → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;

arguments → expression ( "," expression )* ;

primary → "true"
	| "false"
	| NUMBER
	| STRING
	| IDENTIFIER
	| "(" expression ")"
```