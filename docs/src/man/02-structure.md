# 2 Structure of Programs

A REDUCE program consists of a set of functional commands which are evaluated sequentially by the computer. These commands are built up from declarations, statements and expressions. Such entities are composed of sequences of numbers, variables, operators, strings, reserved words and delimiters (such as commas and parentheses), which in turn are sequences of basic characters.

```@contents
Pages = ["02-structure.md"]
```

## 2.1 The REDUCE Standard Character Set

The basic characters which are used to build REDUCE symbols are the following:

1. The 26 letters `a` through `z`
2. The 10 decimal digits `0` through `9`
3. The special characters `_ ! ~ $ % ’ ( ) * + , - . / : ; < > = { }⟨blank⟩`

With the exception of strings and characters preceded by an exclamation mark, the case of characters is ignored: depending of the underlying LISP they will all be converted internally into lower case or upper case: `ALPHA`, `Alpha` and `alpha` represent the same symbol. Most implementations allow you to switch this conversion off. The operating instructions for a particular implementation should be consulted on this point. For portability, we shall limit ourselves to the standard character set in this exposition.

## 2.2 Numbers

There are several different types of numbers available in REDUCE. Integers consist of a signed or unsigned sequence of decimal digits written without a decimal point, for example:
```
-2, 5396, +32
```
In principle, there is no practical limit on the number of digits permitted as exact arithmetic is used in most implementations. (You should however check the specific instructions for your particular system implementation to make sure that this is true.) For example, if you ask for the value of ``2^{2000}`` you get it displayed as a number of 603 decimal digits, taking up several lines of output on an interactive display. It should be borne in mind of course that computations with such long numbers can be quite slow.

Numbers that aren’t integers are usually represented as the quotient of two integers, in lowest terms: that is, as rational numbers.

In essentially all versions of REDUCE it is also possible (but not always desirable!) to ask REDUCE to work with floating point approximations to numbers again, to any precision. Such numbers are called *real*. They can be input in two ways:

1. as a signed or unsigned sequence of any number of decimal digits with an embedded or trailing decimal point.
2. as in 1. followed by a decimal exponent which is written as the letter E followed by a signed or unsigned integer.

e.g. `32. +32.0 0.32E2` and `320.E-1` are all representations of 32.

```@docs
Reduce.Algebra.scientific_notation
```

*CAUTION:* The unsigned part of any number may *not* begin with a decimal point, as this causes confusion with the `CONS` (`.`) operator, i.e., *NOT ALLOWED ARE:* `.5 -.23 +.12`; use `0.5 -0.23 +0.12` instead.

## 2.3 Identifiers

Identifiers in REDUCE consist of one or more alphanumeric characters (i.e. alphabetic letters or decimal digits) the first of which must be alphabetic. The maximum number of characters allowed is implementation dependent, although twenty-four is permitted in most implementations. In addition, the underscore character `_` is considered a letter if it is *within* an identifier. For example,
```
a az p1 q23p  a_very_long_variable
```
are all identifiers, whereas
```
_a
```
is not.

A sequence of alphanumeric characters in which the first is a digit is interpreted as a product. For example, `2ab3c` is interpreted as `2*ab3c`. There is one exception to this: If the first letter after a digit is `E`, the system will try to interpret that part of the sequence as a real number, which may fail in some cases. For example, `2E12` is the real number `2.0 * 1012`, `2e3c` is `2000.0*C`, and `2ebc` gives an error.

Special characters, such as `-`, `*`, and `<blank>`, may be used in identifiers too, even as the first character, but each must be preceded by an exclamation mark in input. For example:
```
light!-years    d!\*!\*n         good! morning  
!$sign          !5goldrings
```
*CAUTION:* Many system identifiers have such special characters in their names (especially `*` and `=`). If the user accidentally picks the name of one of them for his own purposes it may have catastrophic consequences for his REDUCE run. Users are therefore advised to avoid such names.

Identifiers are used as variables, labels and to name arrays, operators and procedures.
Restrictions

The reserved words listed in section [Appendix A: Reserved Identifiers](@ref) may not be used as identifiers. No spaces may appear within an identifier, and an identifier may not extend over a line of text.

## 2.4 Variables

Every variable is named by an identifier, and is given a specific type. The type is of no concern to the ordinary user. Most variables are allowed to have the default type, called scalar. These can receive, as values, the representation of any ordinary algebraic expression. In the absence of such a value, they stand for themselves.

### Reserved Variables

Several variables in REDUCE have particular properties which should not be changed by the user. These variables include:

#### CATALAN

Catalan's constant, defined as

``\sum_{n=0}^\infty \frac{(-1)^n}{(2n+1)^2}.``

#### E

Intended to represent the base of the natural logarithms. `log(e)`, if it occurs in an expression, is automatically replaced by 1. If `rounded` is on, `E` is replaced by the value of ``e`` to the current degree of floating point precision.

#### EULER_GAMMA

```@docs
Reduce.Algebra.euler_gamma
```

#### GOLDEN_RATIO

The number ``\frac{1+\sqrt{5}}{2}``.

#### I

Intended to represent the square root of -1. `i^2` is replaced by -1, and appropriately for higher powers of `I`. This applies only to the symbol `I` used on the top level, not as a formal parameter in a procedure, a local variable, nor in the context `for i:= ...`.

#### INFINITY

Intended to represent ∞ in limit and power series calculations for example, as well as in definite integration. Note however that the current system does not do proper arithmetic on ∞. For example, `infinity + infinity` is `2*infinity`.

#### KHINCHIN

```@docs
Reduce.Algebra.khinchin
```

#### NEGATIVE

Used in the Roots package.

#### NIL

In REDUCE (algebraic mode only) taken as a synonym for zero. Therefore `nil` cannot be used as a variable.

#### PI

Intended to represent the circular constant. With `rounded` on, it is replaced by the value of π to the current degree of floating point precision.

#### POSITIVE

Used in the Roots package.

#### T

Must not be used as a formal parameter or local variable in procedures, since conflict arises with the symbolic mode meaning of `T` as *true*.

Other reserved variables, such as `low_pow`, described in other sections, are listed in [Appendix A: Reserved Identifiers](@ref).

Using these reserved variables inappropriately will lead to errors.

There are also internal variables used by REDUCE that have similar restrictions. These usually have an asterisk in their names, so it is unlikely a casual user would use one. An example of such a variable is `K!\*` used in the asymptotic command package.

Certain words are reserved in REDUCE. They may only be used in the manner intended. A list of these is given in the section “Reserved Identifiers”. There are, of course, an impossibly large number of such names to keep in mind. The reader may therefore want to make himself a copy of the list, deleting the names he doesn’t think he is likely to use by mistake.

## 2.5 Strings

Strings are used in `write` statements, in other output statements (such as error messages), and to name files. A string consists of any number of characters enclosed in double quotes. For example:
```
~A String~
```
Lower case characters within a string are not converted to upper case.

The string `~~` represents the empty string. A double quote may be included in a string by preceding it by another double quote. Thus `~a~~b~` is the string `a~b`, and `~~~~` is the string consisting of the single character `~`.

**Note that the Reduce.jl parser does not currently support REDUCE strings, as there is no need for them due to the native string support of the Julia language.**

## 2.6 Comments

Text can be included in program listings for the convenience of human readers, in such a way that REDUCE pays no attention to it. There are two ways to do this:

1. Everything from the word `comment` to the next statement terminator, normally `;` or `$`, is ignored. Such comments can be placed anywhere a blank could properly appear. (Note that `end` and `>>` are not treated as `comment` delimiters!)
2. Everything from the symbol `%` to the end of the line on which it appears is ignored. Such comments can be placed as the last part of any line. Statement terminators have no special meaning in such comments. Remember to put a semicolon before the `%` if the earlier part of the line is intended to be so terminated. Remember also to begin each line of a multi-line `%` comment with a `%` sign.

## 2.7 Operators

Operators in REDUCE are specified by name and type. There are two types, infix and prefix. Operators can be purely abstract, just symbols with no properties; they can have values assigned (using `:=` or simple `let` declarations) for specific arguments; they can have properties declared for some collection of arguments (using more general `let` declarations); or they can be fully defined (usually by a procedure declaration).

Infix operators have a definite precedence with respect to one another, and normally occur between their arguments. For example:
```
a + b - c   (spaces optional)
x<y and y=z (spaces required where shown)
```
Spaces can be freely inserted between operators and variables or operators and operators. They are required only where operator names are spelled out with letters (such as the `and` in the example) and must be unambiguously separated from another such or from a variable (like `Y`). Wherever one space can be used, so can any larger number.

Prefix operators occur to the left of their arguments, which are written as a list enclosed in parentheses and separated by commas, as with normal mathematical functions, e.g.,
```
cos(u)  
df(x^2,x)  
q(v+w)
```
Unmatched parentheses, incorrect groupings of infix operators and the like, naturally lead to syntax errors. The parentheses can be omitted (replaced by a space following the operator name) if the operator is unary and the argument is a single symbol or begins with a prefix operator name:
```
cos y         means cos(y)
cos (-y)      – parentheses necessary
log cos y     means log(cos(y))
log cos (a+b) means log(cos(a+b))
```
but
```
cos a*b       means (cos a)*b
cos -y        is erroneous (treated as a variable
              “cos” minus the variable y)
```
A unary prefix operator has a precedence higher than any infix operator, including unary infix operators. In other words, REDUCE will always interpret `cos y + 3` as `(cos y) + 3` rather than as `cos(y + 3)`.

Infix operators may also be used in a prefix format on input, e.g., `+(a,b,c)`. On output, however, such expressions will always be printed in infix form (i.e., `a + b + c` for this example).

A number of prefix operators are built into the system with predefined properties. Users may also add new operators and define their rules for simplification. The built in operators are described in another section.

### Built-In Infix Operators

The following infix operators are built into the system. They are all defined internally as procedures.
```
⟨infix operator⟩        where∣:=∣or∣and∣member∣memq∣
                        =∣neq∣eq∣>=∣>∣<=∣<∣
                        +∣-∣*∣/∣^∣**∣.
```
These operators may be further divided into the following subclasses:
```
⟨assignment operator⟩   :=
⟨logical operator⟩      or∣and∣member∣memq
⟨relational operator⟩   =∣neq∣eq∣>=∣>∣<=∣<
⟨substitution operator⟩ where
⟨arithmetic operator⟩   +∣-∣*∣/∣^∣\*\*
⟨construction operator⟩ .
```
`memq` and `eq` are not used in the algebraic mode of REDUCE. They are explained in the section on symbolic mode. `where` is described in the section on substitutions.

In previous versions of REDUCE, *not* was also defined as an infix operator. In the present version it is a regular prefix operator, and interchangeable with *null*.

For compatibility with the intermediate language used by REDUCE, each special character infix operator has an alternative alphanumeric identifier associated with it. These identifiers may be used interchangeably with the corresponding special character names on input. This correspondence is as follows:
```
:=   	setq    	(the assignment operator)
=   	equal    
>=   	geq    
>   	greaterp    
<=   	leq    
<   	lessp    
+   	plus    
-   	difference   	(if unary, minus)
*   	times    
/   	quotient    	(if unary, recip)
^ or ** expt    	(raising to a power)
.   	cons    
```
Note: `neq` is used to mean not equal. There is no special symbol provided for it.

The above operators are binary, except `not` which is unary and `+` and `*` which are nary (i.e., taking an arbitrary number of arguments). In addition, `-` and `/` may be used as unary operators, e.g., `/2` means the same as `1/2`. Any other operator is parsed as a binary operator using a left association rule. Thus `a/b/c` is interpreted as `(a/b)/c`. There are two exceptions to this rule: `:=` and `.` are right associative. Example: `a:=b:=c` is interpreted as `a:=(b:=c)`. Unlike ALGOL and PASCAL, `^` is left associative. In other words, `a^b^c` is interpreted as `(a^b)^c`.

The operators `<`, `<=`, `>`, `>=` can only be used for making comparisons between numbers. No meaning is currently assigned to this kind of comparison between general expressions.

Parentheses may be used to specify the order of combination. If parentheses are omitted then this order is by the ordering of the precedence list defined by the right-hand side of the *⟨infix operator⟩* table at the beginning of this section, from lowest to highest. In other words, `where` has the lowest precedence, and `.` (the dot operator) the highest.
