# Language

## Types

The default types are the following:

- `num`: 64 bit floating point number.
- `str`: utf8 string
- `bool`: boolean type

### Functions

given a comma separated list of types (`T1, T2, ...`), a function type is defined as follows:

```
fn (<type_list>) -> <type>
```

### Structs

#### Definition

structs are defined with the struct keyword, followed by a comma separated list of members, of the
form `<identifier> : <type>`, where the last member can specify an optional comma.

```
struct <name> {
    <member_list>
}
```

#### Construction

Instantiating a struct is done by using the structs name followed by curly brackets and a comma separated list of
expressions for each member in order.

```
<struct_name> { <expressions> }
```

#### Reference semantics

Structs are always allocated on the heap and their type is implicitly a reference, meaning that passing a struct
instance to a function or assigning it to a variable does not copy its members.

```
struct MyNum { value: num }

fn f(my_num: MyNum) {
  my_num.value = 1;
}

var x = MyNum{0};
f(x);
assert(x.value == 1);

var y = x;
y.value = 2;
assert(x.value == 2);
```

## Scopes and name resolution

Variable identifiers are bound to their most recent declaration still in scope. Same scope re-declarations are not
allowed

```
var x = "global";
{
    var x = "local";
    assert(x == "local");  
}
assert(x == "global");
```

### Overloading

Global functions are added to a set of functions with the same name. These function have to differ in their parameters,
they cannot be overloaded based on their return type

```
fn add(a: num, b: num) -> num {
    return a + b;
}

fn add(a: str, b: str) -> str {
    return a + b;
}
```

If an overload set contains only one function, it is chosen implicitly, meaning it can be assigned to a variable

```
fn get_platform() -> str { return "android"; }

var f = get_platform;

assert(f() == "android");
```

## Expressions

### Binary operations

in order of precedence (increasing), the binary operators in the language are the following:

- `or`
- `and`
- `<` `>` `<=` `>=`
- `==` `!=`
- `+` `-`
- `*` `/`

by default, the language defines the following operators, defined in terms of the native rust types backing them.

| operator | left hand side type | right hand side type | result type | description            |
|----------|---------------------|----------------------|-------------|------------------------|
| +        | `num`               | `num`                | `num`       | `f64` addition         |
| -        | `num`               | `num`                | `num`       | `f64` subtraction      |
| *        | `num`               | `num`                | `num`       | `f64` multiplication   |
| /        | `num`               | `num`                | `num`       | `f64` division         |
| <        | `num`               | `num`                | `num`       | `f64` less             |
| <=       | `num`               | `num`                | `num`       | `f64` less or equal    |
| \>       | `num`               | `num`                | `num`       | `f64` greater          |
| \>=      | `num`               | `num`                | `num`       | `f64` greater or equal |
| ==       | `num`               | `num`                | `num`       | `f64` equal            |
| !=       | `num`               | `num`                | `num`       | `f64` not equal        |
| +        | `str`               | `str`                | `str`       | `String` concatenation |
| <=       | `num`               | `num`                | `num`       | `f64` less or equal    |
| \>       | `num`               | `num`                | `num`       | `f64` greater          |
| \>=      | `num`               | `num`                | `num`       | `f64` greater or equal |
| ==       | `num`               | `num`                | `num`       | `f64` equal            |
| !=       | `num`               | `num`                | `num`       | `f64` not equal        |

### Unary operators

### Function calls

### Member access

### Assignment

Assignments change the value of a variable, global, local or a capture for a lambda

```
<l-value> = <value>
```

the left side needs to be an l-value, meaning a variable identifier or a struct member.

name resolution for the left hand side is done according to the rules specified in [Name Resolution]()

### Lambdas

Lambdas are functions that can capture their environment. The syntax is the following.

```
fn (<parameters>) -> <return type> {
    <instructions>
};
```

parameters are specified as `name: type`.

The return type is optional (in which case the body can only return the nothing type, but it is not required), but if
specified, the function
needs to return the specified type in all cases

```
// invalid as the return is gated by the if
fn () -> num {
    if (1 > 2) {
        return 0; 
    }
};

// valid as it returns in all cases
fn () -> num {
    if (1 > 2) {
        return 1;
    } else {
        return 2;
    }
};
```

## Variables

variables are declared using the `var` keyword followed by the name, and an optional type

```
var <name> = init_expr;
var <name> : <type> = init_expr;
```

## Modules

### Declaration

a module is declared using the `module` keyword at the start of the file

```
module <module_name>;
```

the declaration can only appear as the first statement. Later programs can import a previously loaded module using
the `import` keyword which bring in scope all module declarations (variables, functions and types).

values changed

### Loading modules

When a module is loaded global instructions, are executed and global variables are initialized. Importing a module does
not execute any code

### Temporary Modules

If a program is missing the module declaration it is called temporary and none of its declarations are exported. There
is also no way to import a temporary module.

## Grammar

### Program

```text
program → declaration* EOF ;
```

### Types

```text

type → IDENTIFIER
    | fn "(" ( type ("," type)*)? ")" "->" type
```

### Declarations

```text
declaration → struct_decl
	| fn_decl
	| var_decl
	| statement;

struct_decl → "struct" IDENTIFIER "{" IDENTIFIER ":" type ("," IDENTIFIER ":" type)* (",")? "}" ;

fn_decl → "fn" IDENTIFIER "(" parameters? ")" -> IDENTIFIER (block | ";") ;

parameters → IDENTIFIER ":" IDENTIFIER ( "," IDENTIFIER ":" type)* ;

var_decl → "var" IDENTIFIER (":" type)? "=" expression  ";" ;
```

### Statements

```text
statement → expr_stmt
	| for_stmt
	| if_stmt
	| print_stmt
	| return_stmt
	| while_stmt
	| block
	| macro_stmt 
	;

expr_stmt → expression ";" ;

for_stmt → "for" "(" IDENTIFIER in IDENTIFIER ")" statement ;

if_stmt → "if" "(" expression ")" statement ( "else" statement )? ;

return_stmt → "return" expression? ";" ;

while_stmt → "while" "(" expression ")" statement ;

block → "{" declaration* "}" ;

macro_stmt → "#" "[" IDENTIFIER "]" declaration ;
```

### Expressions

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