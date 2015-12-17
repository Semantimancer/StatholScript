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

## <a name="let"></a>Let Statements

Let statements follow the form `let foo = bar in baz`, where `foo` is some identifier and `bar` and `baz` are expressions. The identifier given in the let statement does not have to be used within either expression (you can have "useless" lets).

### Polymorphic Types

Beyond their convenience, let statements also serve another purpose. They are the only way to implement polymorphism in StatholScript. When a let statement is typechecked, the first identifier is made into a polymorphic type which is then instantiated individually each time it appears in an expression.
