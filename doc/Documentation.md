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
  <dd>Comparison operaturs take two Num expressions and return a Bool. Note that >= and => are the same, as are <= and =<.</dd>

  <dt>( == )</dt>
  <dd>The equality operator takes any two expressions and returns a Bool that represents whether or not they are functionally equivalent. In Num expressions, equality does not differentiate between different expressions of the same number (e.g. 1==1.0).

  For other uses of `==`, see that section of the documentation.</dd>

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
</dl>

## Lists

Lists are sequences of elements which must all be of the same type. They are of the form `[foo,bar]`, which is also how they are expressed by the typechecker. The empty list is `[]`, which is initially a mutable type.

<dl>
  <dt>head</dt>
  <dd>Returns the first element of a list. If given an empty list, it will instead return an error value.</dd>

  <dt>tail</dt>
  <dd>Returns everything but the first element of a list. If given an empty list, will return the empty list.</dd>

  <dt>null?</dt>
  <dd>Returns true if the list is empty and false otherwise. Note that the parser translates `null? foo` as `foo==[]`.</dd>

  <dt>( : )</dt>
  <dd>Takes an value of type `t` and a `t` list, and prepends the value to the list. Infix operator.</dd>

  <dt>( == )</dt>
  <dd>Two lists are equivalent if they have all the same elements, in the same order.</dd>
</dl>

## Functions

## Let Statements
