# Documentation

## Primitives

StatholScript has two primitive values: Num and Bool. 

Nums are stored as floats, and can be expressed as either normally (e.g. `1`, `1.`, or `1.0`) or through exponential notation (e.g. `1e0` or `1E0`). Exponential notation does allow for negative exponents, but does not allow for spaces: so `1e-2` is valid, but `1e -2` is not. Negative numbers can be expressed, but must be enclosed by parentheses for sanity reasons.

Bools are `true` and `false`.

### Primitive Operators

All operators are infix operators unless otherwise specified.

**(+) (-) (/) (*)**
Arithmetic operators take two Num expressions and return another. In the case of division by zero, it will instead return an error value.

**(&&) (||)**
Boolean operators take two Bool expressions and return a third. They are not bitwise operators.

**(>) (<) (>=) (<=) (=>) (=<)**
Comparison operaturs take two Num expressions and return a Bool. Note that `>=` and `=>` are the same, as are `<=` and `=<`.

**(==)**
The equality operator takes any two expressions and returns a Bool that represents whether or not they are functionally equivalent. In Num expressions, equality does not differentiate between different expressions of the same number (e.g. 1==1.0).

For other uses of `==`, see that section of the documentation.

## If Statements

## Pairs

## Lists

## Functions

## Let Statements
