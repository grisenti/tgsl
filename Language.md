# Hot-reloading
if the line being executed does not change, restart execution as normal, otherwise have to restart

# Environment
resolve all bindings during parsing and assign an id to each named identifier (function or variable). The id is then used in the interpreter to get the value of the identifier from an array

# Syntax
## Variables
```rs
var x = 0;
vax x; // cannot be used, only initialized
```

## Functions
```rs
fn combine(x, y, op) { // define via function declaration
	return op(x, y);
}

combine(x: 1, y: 2, op: fn (x, y) {x + y}) // optional named arguments

var add = fn (x, y) { // define assigning the closure to a variable
	return x + y;
}

//uniform function call syntax
print 1.add(2); // 3
"hello".add(" world!"); // hello world!
```

## Built-in types
```rs
"string"; // dynamic string
1; // f64
true; // bool
[1,2,3]; // array
map["a": 1, "b": 2]; // map
set["a", "b", "c"]; // set
null; // no value
```

## User defined types
```rs
struct A {
	a,
	b,
	c,
}

var a = A(1,2,3) // initialization

fn reduce (a) {a.a + a.b + a.c}

a.reduce() // uniform function call syntax again
```

## String formatting
```rs
var hello = "HELLO";
print "{hello.to_lower()}"; // hello
```

## Compiler directives
```rs
#[enable(set)]
```

# Grammar
## Program
```text
program → declaration* EOF ;
```
## Declarations
```text
declaration → structDecl
	| funDecl
	| varDecl
	| statement ;

structDecl → "struct" IDENTIFIER "{" (IDENTIFIER ",")* "}";

funcDecl → "fn" IDENTIFIER function

parameters → IDENTIFIER ( "," IDENTIFIER  )* ;

idDecl → "var" IDENTIFIER ( "=" expression )? ";" | "const" IDENTIFIER "=" expression ";" ;
```

## Statements
```text
statement → exprStmt
	| forStmt
	| ifStmt
	| printStmt
	| returnStmt
	| whileStmt
	| block ;

exprStmt → expression ";" ;

forStmt → "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement ;

ifStmt → "if" "(" expression ")" statement ( "else" statement )? ;

printStmt → "print" expression ";" ;

returnStmt → "return" expression? ";" ;

whileStmt → "while" "(" expression ")" statement ;

block → "{" declaration* "}" ;
```

## Expressions
```
expression → closure | assignment;

closure → "fn" function;

assignment → ( call "." )? IDENTIFIER "=" assignment
	| logic_or ;

logic_or → logic_and ( "or" logic_and )* ;

logic_and → equality ( "and" equality )* ;

equality → comparison ( ( "!=" | "==" ) comparison )* ;

comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;

term → factor ( ( "-" | "+" ) factor )* ;

factor → unary ( ( "/" | "*" ) unary

unary → ( "!" | "-" ) unary | call ;

call → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;

primary → "true"
	| "false"
	| NUMBER
	| STRING
	| IDENTIFIER
	| "(" expression ")"
```

## Others
```text
function → "(" parameters? ")" block;
arguments → expression ( "," expression )* ;
```

