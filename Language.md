# Hot-reloading
if the line being executed does not change, restart execution as normal, otherwise have to restart

# Environment
resolve all bindings during parsing and assign an id to each named identifier (function or variable). The id is then used in the interpreter to get the value of the identifier from an array

# Syntax
## Functions
```rs
// untyped
fn add(x, y) {
	return x + y;
}
//typed
fn sum(x: num, y: num) -> num {
	return x + y;
}
```

## Variables
```rs
//infered
var x = 0;
//specified
var x: num = 0;
```

## Built-in types
```rs
var a = "string"; // dynamic string
var b = 1; // f64 
var c = [1,2,3]; // array
var d = map["a": 1, "b": 2]; // map
var e = set["a", "b", "c"]; // set
```

## User defined types
```rs
struct A {
	a, //any type
	b: num,
	c: str,
}

fn to_string(A) -> string {
	A.a
}

var a = A(0, 2, "hello");
print a.to_string();
```

## String formatting
```rs
var id = "hello";
var a = A(a, b, c);
"{id} - {a.to_string()}"
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
declaration → classDecl
	| funDecl
	| varDecl
	| statement ;

classDecl → "class" IDENTIFIER ( "<" IDENTIFIER )? "{" function* "}" ;

funDecl → "fun" function ;

varDecl → "var" IDENTIFIER  ( "=" expression )? ";" ;
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
expression → assignment ;

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
	| "nil"
	| "this"
	| NUMBER
	| STRING
	| IDENTIFIER
	| "(" expression ")"
	| "super" "." IDENTIFIER ;
```

## Others
```text
function → IDENTIFIER "(" parameters? ")" block ;

parameters → IDENTIFIER ( "," IDENTIFIER  )* ;

arguments → expression ( "," expression )* ;

type_specifier → (":" IDENTIFIER)?
```

