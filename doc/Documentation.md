# Documentation

## Primitives

StatholScript has two primitive values: Num and Bool. 

Nums are stored as floats, and can be expressed as either normally (e.g. `1`, `1.`, or `1.0`) or through exponential notation (e.g. `1e0` or `1E0`). Exponential notation does allow for negative exponents, but does not allow for spaces: so `1e-2` is valid, but `1e -2` is not. Negative numbers can be expressed, but must be enclosed by parentheses for sanity reasons.

Bools are `true` and `false`.

### Primitive Operators

All operators are infix operators unless otherwise specified. They are also all left-associative.

<dl>
  <dt>( + ) ( - ) ( / ) ( * )</dt>
  <dd>Arithmetic operators take two Num expressions and return another. In the case of division by zero, it will instead return an error value.</dd>

  <dt>( && ) ( || )</dt>
  <dd>Boolean operators take two Bool expressions and return a third. They are not bitwise operators.</dd>

  <dt>( > ) ( < ) ( >= ) ( <= ) ( => ) ( =< )</dt>
  <dd>Comparison operaturs take two Num expressions and return a Bool. Note that <code>>=</code> and <code>=></code> are the same, as are <code><=</code> and <code>=<</code>.</dd>

  <dt>( == )</dt>
  <dd>The equality operator takes any two expressions and returns a Bool that represents whether or not they are functionally equivalent. In Num expressions, equality does not differentiate between different expressions of the same number (e.g. <code>1==1.0</code>).   

  For other uses of <code>==</code>, see that section of the documentation.</dd>

  <dt>! not</dt>
  <dd>Not operators take a Bool expression and return the opposite. They are not infix.</dd>
</dl>

## If Statements

If statements take the form `if foo then bar else baz`. Both the then and else branches are required. `foo` must be a Bool expression, and `bar` and `baz` must have the same type.

## Pairs

Pairs are tuples of the form `(foo,bar)`, which is also how they are expressed by the typechecker. The two elements in a list do not have to be the same type. More advanced forms (triples, quadruples, etc) are not natively supported; but they can be emulated with nesting tuples.

<dl>
  <dt>fst</dt>
  <dd>Returns the first element of a pair. Right-associative.</dd>

  <dt>snd</dt>
  <dd>Returns the second element of a pair. Right-associative.</dd>

  <dt>( == )</dt>
  <dd>Two pairs are equivalent if their first elements are equivalent and their second elements are equivalent.   

  For other uses of <code>==</code>, see that section of the documentation.
</dl>

## Lists

Lists are sequences of elements which must all be of the same type. They are of the form `[foo,bar]`, which is also how they are expressed by the typechecker. The empty list is `[]`, which is initially a mutable type.

<dl>
  <dt>head</dt>
  <dd>Returns the first element of a list. If given an empty list, it will instead return an error value.</dd>

  <dt>tail</dt>
  <dd>Returns everything but the first element of a list. If given an empty list, will return the empty list.</dd>

  <dt>null?</dt>
  <dd>Returns true if the list is empty and false otherwise. Note that the parser translates <code>null? foo</code> as <code>foo==[]</code>.</dd>

  <dt>( : )</dt>
  <dd>Takes an value of type <code>t</code> and a <code>t</code> list, and prepends the value to the list. Infix operator.</dd>

  <dt>( == )</dt>
  <dd>Two lists are equivalent if they have all the same elements, in the same order.   
  
  For other uses of <code>==</code>, see that section of the documentation.</dd>
</dl>

## Functions

The standard way to define a function is as an anonymous (lambda) function, which takes the form `(\foo -> bar)`. The parentheses are required. Immediately after the `\` is the list of parameters, followed by a `->` and then the expression itself.    

Parameters to a function are treated individually, and are interpreted before they are passed to the function. They can be of any value, including Closure, so it is possible to pass functions to other functions.

Function definitions on their own return a Closure value, and are expressed by the typechecker as `(foo -> bar)`. 

<dl>
  <dt>( == )</dt>
  <dd>Two Closure values are never equivalent. This will always return false.

  For other uses of <code>==</code>, see that section of the documentation.</dd>
</dl>

### Typechecking Functions

During typechecking, each parameter in `foo` will be given its own unique, generic type which may then be made non-generic when the second part of the function definition is checked.

### Partial Application

Partial application is allowed, and even made explicit within the typechecker. For example, `(\x y -> x+y)` is a Closure of type `(Num -> (Num -> Num))`, but `(\x y -> x+y) 1` is a Closure of type `(Num -> Num)`.

### Recursive Functions

Lambda functions are not inherently given a name, and therefore cannot be used recursively. However, recursive functions can be implemented by giving a Closure value an identifier as part of a [let statement](#let).

#### Higher Order Functions

Higher order functions can be implemented in StatholScript using let statements. For example:

```
let map = (\f l -> if null? l then [] else (f (head l)):(map f (tail l))) in ...

let filter = (\f l -> if null? l then [] else if f (head l) then (head l):(filter f (tail l)) else filter f (tail l)) in ...

let fold = (\f acc l -> if null? l then acc else fold f (f acc (head l)) (tail l)) in ...
```

### Function Application

Application is as simple as `foo bar`, where foo is a Closure value and bar is a value of the proper type. There is, however, a sequence operator.

<dl>
  <dt>( |> )</dt>
  <dd>The sequence operator takes expressions of type <code>foo |> bar</code> and applies <code>bar</code> to the output of <code>foo</code>. This is a left-associative infix operator. For example, <code>fst (1,2) |> (\x -> x+1) |> (\x -> x+1)</code> returns 3.</dd>
</dl>

## <a name="let"></a>Let Statements

Let statements follow the form `let foo = bar in baz`, where `foo` is some identifier and `bar` and `baz` are expressions. The identifier given in the let statement does not have to be used within either expression (you can have "useless" lets).

### Polymorphic Types

Beyond their convenience, let statements also serve another purpose. They are the only way to implement polymorphism in StatholScript. When a let statement is typechecked, the first identifier is made into a polymorphic type which is then instantiated individually each time it appears in an expression.

### Equivalence

The equivalence operator (`==`) does not differentiate between normal expressions and identifiers in a let statement. It does not, therefore, automatically return true if given the same identifier twice.   

So while `let x = 1 in x==x` returns true, `let f = (\x -> x) in f==f` does not.

## The Interpreter

The interpreter packaged with StatholScript is designed to provide useful information and to catch errors in an expressive, non-fatal way (i.e. without crashing). After being given a valid expression, the interpreter will return the value of that expression and, in the next prompt line, will also return the type of that expression. For example:

```
$ ./lang
>> 1;
1.
{ Num }>> true;
true
{ Bool }>> [1,1];
[1.,1.,]
{ [Num] }>>
```

If the interpreter catches an error in the expression, it will try to return an expressive error. For instance:

```
$ ./lang
>> 1==true;
Num ?= Bool
Typecheck error: Bad constraints
```

The line `Num ?= Bool` means that the typechecker tried to validate a constraint which would have required a Num value and a Bool value to be of the same type. 

### Line Breaks

StatholScript requires a semicolon after every line. The interpreter reads line breaks the same way it reads whitespace (i.e. it ignores it), so you can spread your expression out across multiple lines without any trouble as long as you remember to end it with a `;`.

## Exceptions

One of the goals when making StatholScript was to create a language/interpreter which would never suffer a fatal error. This is achieved by having two different types of errors: those picked up by the interpreter and those picked up by the driver.

Interpreter errors are for errors which cannot be picked up by the lexer, parser, or typechecker. These are errors that only arise because //certain// values were used. For example, both `head []` and `3/0` will produce interpreter errors. These errors will always contain the phrase `Error In Value`.

Driver errors are errors that are caught before the interpreter even has a chance to begin running. There are a number of these errors:
  * **Lexer Error**: This is raised when the lexer finds a phrase that it cannot recognize; this is normally due to misplaced spaces or bad variable names. It will return `Unrecognized token error`.
  * **Parse Error**: This is raised when the parser is given an invalid statement using only tokens that were recognized by the lexer. It will return `Parse error in statement`.
  * **Typecheck Error**: This is raised when the typechecker finds something which it is not supposed to allow through. There are several types of typecheck errors, each with their own return statement.
  * **Desugarer Error**: This is raised when the desugarer finds a phrase that exists in the surface language but not in the core. It is included in this list for completeness's sake, but should never actually be raised.
  * **Self-Referential Error**: StatholScript does not allow an expression to use itself within its own definition (this is why recursive functions require let statements). If such an expression is found, it will raise a `Self-referential error`.
  * **Other Error**: Just in case an error was missed, the interpreter also has a "blanket" catch. If this happens, it will raise `You've encountered an unexpected error.` If you find this error, the maintainer of this repository would be very interested to hear what expression you used to get it.
