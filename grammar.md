# Grammar

## Program

```text
program → declaration* EOF ;
```

## Types

```text

type → IDENTIFIER
    | fn "(" ( type ("," type)*)? ")" "->" type
```

## Declarations

```text
declaration → struct_decl
	| fn_decl
	| var_decl
	| statement;

struct_decl → "struct" IDENTIFIER "{" (IDENTIFIER ":" type ",")* "}";

fn_decl → "fn" IDENTIFIER "(" parameters? ")" -> IDENTIFIER (block | ;)

parameters → IDENTIFIER ":" IDENTIFIER ( "," IDENTIFIER ":" type)* ;

var_decl → "var" IDENTIFIER (":" type)? ( "=" expression )? ";";
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

lambda → "fn" "(" parameters? ")" -> type block;

assignment → (IDENTIFIER | member_get) "=" expression
	| logic_or ;

logic_or → logic_and ( "or" logic_and )* ;

logic_and → equality ( "and" equality )* ;

equality → comparison ( ( "!=" | "==" ) comparison )* ;

comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;

term → factor ( ( "-" | "+" ) factor )* ;

factor → unary (( "/" | "*" ) unary)*;

unary → ( "!" | "-" ) unary | call ;

call → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;

fn_call → primary "(" arguments? ")" ;

member_get → primary "." IDENTIFIER ;  

dot_call → primary "." IDENTIFIER "(" arguments? ")" ;

arguments → expression ( "," expression )* ;

primary → "true"
	| "false"
	| NUMBER
	| STRING
	| IDENTIFIER
	| "(" expression ")"
	| constructor
	| map
	| array
	
constructor → IDENTIFIER "{" arguments "}"

map → "{" (expression ":" expression ("," expression ":" expression))? "}"

array → "["  (expression ("," expression)*)? "]"
```

