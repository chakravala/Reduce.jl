# 9 Polynomials and Rationals

Many operations in computer algebra are concerned with polynomials and rational functions. In this section, we review some of the switches and operators available for this purpose. These are in addition to those that work on general expressions (such as `df` and `int`) described elsewhere. In the case of operators, the arguments are first simplified before the operations are applied. In addition, they operate only on arguments of prescribed types, and produce a type mismatch error if given arguments which cannot be interpreted in the required mode with the current switch settings. For example, if an argument is required to be a kernel and `a/2` is used (with no other rules for `a`), an error
```
        A/2 invalid as kernel
```
will result.

With the exception of those that select various parts of a polynomial or rational function, these operations have potentially significant effects on the space and time associated with a given calculation. The user should therefore experiment with their use in a given calculation in order to determine the optimum set for a given problem.

One such operation provided by the system is an operator `length` which returns the number of top level terms in the numerator of its argument. For example,
```Julia
julia> Algebra.length(:((a+b+c)^3/(c+d)))
```
has the value 10. To get the number of terms in the denominator, one would first select the denominator by the operator `den` and then call `length`, as in
```
julia> Algebra.length(Algebra.den(:((a+b+c)^3/(c+d))))
```
Other operations currently supported, the relevant switches and operators, and the required argument and value modes of the latter, follow.

```@contents
Pages = ["09-polynomials.md"]
```

## 9.1 Controlling the Expansion of Expressions

The switch `exp` controls the expansion of expressions. If it is off, no expansion of powers or products of expressions occurs. Users should note however that in this case results come out in a normal but not necessarily canonical form. This means that zero expressions simplify to zero, but that two equivalent expressions need not necessarily simplify to the same form.

*Example:* With `exp` on, the two expressions
```
(a+b)*(a+2*b)
```
and
```
a^2+3*a*b+2*b^2
```
will both simplify to the latter form. With `exp` off, they would remain unchanged, unless the complete factoring (`allfac`) option were in force. `exp` is normally on.

**Note that in Reduce.jl `exp` is turned off by default on initialization**

Several operators that expect a polynomial as an argument behave differently when `exp` is off, since there is often only one term at the top level. For example, with `exp` off
```
julia> Algebra.length(:((a+b+c)^3/(c+d)))
```
returns the value 1.

## 9.2 Factorization of Polynomials

REDUCE is capable of factorizing univariate and multivariate polynomials that have integer coefficients, finding all factors that also have integer coefficients. The package for doing this was written by Dr. Arthur C. Norman and Ms. P. Mary Ann Moore at The University of Cambridge. It is described in P. M. A. Moore and A. C. Norman, “Implementing a Polynomial Factorization and GCD Package”, Proc. SYMSAC ’81, ACM (New York) (1981), 109-116.

The easiest way to use this facility is to turn on the switch `factor`, which causes all expressions to be output in a factored form. For example, with `factor` on, the expression `a^2-b^2` is returned as `(a+b)*(a-b)`.

```@docs
Reduce.Algebra.factorize
```

The order in which the factors occur in the result (with the exception of a possible overall numerical coefficient which comes first) can be system dependent and should not be relied on. Similarly it should be noted that any pair of individual factors can be negated without altering their product, and that REDUCE may sometimes do that.

The factorizer works by first reducing multivariate problems to univariate ones and then solving the univariate ones modulo small primes. It normally selects both evaluation points and primes using a random number generator that should lead to different detailed behavior each time any particular problem is tackled. If, for some reason, it is known that a certain (probably univariate) factorization can be performed effectively with a known prime, `p` say, this value of `p` can be handed to `factorize` as a second argument. An error will occur if a non-prime is provided to `factorize` in this manner. It is also an error to specify a prime that divides the discriminant of the polynomial being factored, but users should note that this condition is not checked by the program, so this capability should be used with care.

Factorization can be performed over a number of polynomial coefficient domains in addition to integers. The particular description of the relevant domain should be consulted to see if factorization is supported. For example, the following statements will factorize ``x^4 + 1`` modulo 7:
```Julia
Algebra.setmod(7)
Algebra.on(:modular)
Algebra.factorize(:(x^4+1))
```
The factorization module is provided with a trace facility that may be useful as a way of monitoring progress on large problems, and of satisfying curiosity about the internal workings of the package. The most simple use of this is enabled by issuing the REDUCE command `on(:trfac)`. Following this, all calls to the factorizer will generate informative messages reporting on such things as the reduction of multivariate to univariate cases, the choice of a prime and the reconstruction of full factors from their images. Further levels of detail in the trace are intended mainly for system tuners and for the investigation of suspected bugs. For example, `trallfac` gives tracing information at all levels of detail. The switch that can be set by `on(:timings)` makes it possible for one who is familiar with the algorithms used to determine what part of the factorization code is consuming the most resources. `on(:overview)` reduces the amount of detail presented in other forms of trace. Other forms of trace output are enabled by directives of the form
```
        symbolic set!-trace!-factor(<number>,<filename>);
```
where useful numbers are 1, 2, 3 and 100, 101, ... . This facility is intended to make it possible to discover in fairly great detail what just some small part of the code has been doing — the numbers refer mainly to depths of recursion when the factorizer calls itself, and to the split between its work forming and factorizing images and reconstructing full factors from these. If `nil` is used in place of a filename the trace output requested is directed to the standard output stream. After use of this trace facility the generated trace files should be closed by calling
```
        symbolic close!-trace!-files();
```
*NOTE:* Using the factorizer with `mcd` off will result in an error.

## 9.3 Cancellation of Common Factors

Facilities are available in REDUCE for cancelling common factors in the numerators and denominators of expressions, at the option of the user. The system will perform this greatest common divisor computation if the switch `gcd` is on. (`gcd` is normally off.)

A check is automatically made, however, for common variable and numerical products in the numerators and denominators of expressions, and the appropriate cancellations made.

When `gcd` is on, and `exp` is off, a check is made for square free factors in an expression. This includes separating out and independently checking the content of a given polynomial where appropriate. (For an explanation of these terms, see Anthony C. Hearn, “Non-Modular Computation of Polynomial GCDs Using Trial Division”, Proc. EUROSAM 79, published as Lecture Notes on Comp. Science, Springer-Verlag, Berlin, No 72 (1979) 227-239.)

*Example:* With `exp` off and `gcd` on, the polynomial `a*c+a*d+b*c+b*d` would be returned as `(a+b)*(c+d)`.

Under normal circumstances, GCDs are computed using an algorithm described in the above paper. It is also possible in REDUCE to compute GCDs using an alternative algorithm, called the EZGCD Algorithm, which uses modular arithmetic. The switch `ezgcd`, if on in addition to `gcd`, makes this happen.

In non-trivial cases, the EZGCD algorithm is almost always better than the basic algorithm, often by orders of magnitude. We therefore *strongly* advise users to use the `ezgcd` switch where they have the resources available for supporting the package.

For a description of the EZGCD algorithm, see J. Moses and D.Y.Y. Yun, “The EZ GCD Algorithm”, Proc. ACM 1973, ACM, New York (1973) 159-166.

*NOTE:* This package shares code with the factorizer, so a certain amount of trace information can be produced using the factorizer trace switches.

An implementation of the heuristic GCD algorithm, first introduced by B.W. Char, K.O. Geddes and G.H. Gonnet, as described in J.H. Davenport and J. Padget, “HEUGCD: How Elementary Upperbounds Generate Cheaper Data”, Proc. of EUROCAL ’85, Vol 2, 18-28, published as Lecture Notes on Comp. Science, No. 204, Springer-Verlag, Berlin, 1985, is also available on an experimental basis. To use this algorithm, the switch `heugcd` should be on in addition to `gcd`. Note that if both `ezgcd` and `heugcd` are on, the former takes precedence.

### 9.3.1 Determining the GCD of Two Polynomials

This operator, used with the syntax
```Julia
R"gcd(EXPRN1:polynomial,EXPRN2:polynomial)"
```
returns the greatest common divisor of the two polynomials `EXPRN1` and `EXPRN2`.
*Examples:*
```Julia
julia> Algebra.gcd(:(x^2+2*x+1),:(x^2+3*x+2))
:(x + 1)

julia> Algebra.gcd(:(2*x^2-2*y^2),:(4*x+4*y))
:(2 * (x + y))

julia> Algebra.gcd(:(x^2+y^2),:(x-y))
1
```

## 9.4 Working with Least Common Multiples

Greatest common divisor calculations can often become expensive if extensive work with large rational expressions is required. However, in many cases, the only significant cancellations arise from the fact that there are often common factors in the various denominators which are combined when two rationals are added. Since these denominators tend to be smaller and more regular in structure than the numerators, considerable savings in both time and space can occur if a full GCD check is made when the denominators are combined and only a partial check when numerators are constructed. In other words, the true least common multiple of the denominators is computed at each step. The switch `lcm` is available for this purpose, and is normally on.

In addition, the operator `lcm`, used with the syntax
```Julia
R"lcm(EXPRN1:polynomial,EXPRN2:polynomial)"
```
returns the least common multiple of the two polynomials `EXPRN1` and `EXPRN2`.

*Examples:*
```Julia
julia> Algebra.lcm(:(x^2+2*x+1),:(x^2+3*x+2))
:((x + 2) * (x + 1) ^ 2)

julia> Algebra.lcm(:(2*x^2-2*y^2),:(4*x+4*y))
:(4 * (x ^ 2 - y ^ 2))
```

## 9.5 Controlling Use of Common Denominators

When two rational functions are added, REDUCE normally produces an expression over a common denominator. However, if the user does not want denominators combined, he or she can turn off the switch `mcd` which controls this process. The latter switch is particularly useful if no greatest common divisor calculations are desired, or excessive differentiation of rational functions is required.

*CAUTION:* With `mcd` off, results are not guaranteed to come out in either normal or canonical form. In other words, an expression equivalent to zero may in fact not be simplified to zero. This option is therefore most useful for avoiding expression swell during intermediate parts of a calculation.

`mcd` is normally on.

## 9.6 REMAINDER Operator

```@docs
Reduce.Algebra.remainder
```

*CAUTION:* In the default case, remainders are calculated over the integers. If you need the remainder with respect to another domain, it must be declared explicitly.

*Example:*
```
        remainder(x^2-2,x+sqrt(2)); -> x^2 - 2  
        load_package arnum;  
        defpoly sqrt2**2-2;  
        remainder(x^2-2,x+sqrt2); -> 0
```

## 9.7 RESULTANT Operator

```@docs
Reduce.Algebra.resultant
```

The sign conventions used by the resultant function follow those in R. Loos, “Computing in Algebraic Extensions” in “Computer Algebra — Symbolic and Algebraic Computation”, Second Ed., Edited by B. Buchberger, G.E. Collins and R. Loos, Springer-Verlag, 1983. Namely, with `a` and `b` not dependent on `x`:
```
                               deg(p)*deg(q)  
   resultant(p(x),q(x),x)= (-1)             *resultant(q,p,x)  
 
                            deg(p)  
   resultant(a,p(x),x)   = a  
 
   resultant(a,b,x)      = 1
```
*Examples:*
```Julia
julia> Algebra.resultant(:(x/r*u+y),:(u*y),:u)
:(-(y ^ 2))
```
calculation in an algebraic extension:
```
   load arnum;  
   defpoly sqrt2**2 - 2;  
 
   resultant(x + sqrt2,sqrt2 * x +1,x) -> -1
```
or in a modular domain:
```
julia> Algebra.setmod(17); Algebra.on(:modular);
 
julia> Algebra.resultant(:(2x+1),:(3x+4),:x)
5
```

## 9.8 DECOMPOSE Operator

```@docs
Reduce.Algebra.decompose
```

## 9.9 INTERPOL operator

```@docs
Reduce.Algebra.interpol
```

## 9.10 Obtaining Parts of Polynomials and Rationals

These operators select various parts of a polynomial or rational function structure. Except for the cost of rearrangement of the structure, these operations take very little time to perform.

For those operators in this section that take a kernel `VAR` as their second argument, an error results if the first expression is not a polynomial in `VAR`, although the coefficients themselves can be rational as long as they do not depend on `VAR`. However, if the switch `ratarg` is on, denominators are not checked for dependence on `VAR`, and are taken to be part of the coefficients.

```@docs
Reduce.Algebra.deg
Reduce.Algebra.den
Reduce.Algebra.lcof
Reduce.Algebra.lpower
Reduce.Algebra.lterm
```

*Compatibility Note:*  In some earlier versions of REDUCE, `lterm` returned 0 if the `EXPRN` did not depend on `VAR`. In the present version, `EXPRN` is always equal to `lterm(EXPRN,VAR) + reduct(EXPRN,VAR)`.

```@docs
Reduce.Algebra.mainvar
Reduce.Algebra.num
Reduce.Algebra.reduct
```

*Compatibility Note:*  In some earlier versions of REDUCE, `reduct` returned `EXPRN` if it did not depend on `VAR`. In the present version, `EXPRN` is always equal to `lterm(EXPRN,VAR) + reduct(EXPRN,VAR)`.

```@docs
Reduce.Algebra.totaldeg
```

## 9.11 Polynomial Coefficient Arithmetic

REDUCE allows for a variety of numerical domains for the numerical coefficients of polynomials used in calculations. The default mode is integer arithmetic, although the possibility of using real coefficients has been discussed elsewhere. Rational coefficients have also been available by using integer coefficients in both the numerator and denominator of an expression, using the `on(:div)` option to print the coefficients as rationals. However, REDUCE includes several other coefficient options in its basic version which we shall describe in this section. All such coefficient modes are supported in a table-driven manner so that it is straightforward to extend the range of possibilities. A description of how to do this is given in R.J. Bradford, A.C. Hearn, J.A. Padget and E. Schrüfer, “Enlarging the REDUCE Domain of Computation,” Proc. of SYMSAC ’86, ACM, New York (1986), 100–106.

### 9.11.1 Rational Coefficients in Polynomials

Instead of treating rational numbers as the numerator and denominator of a rational expression, it is also possible to use them as polynomial coefficients directly. This is accomplished by turning on the switch `rational`.

*Example:* With `rational` off, the input expression `a/2` would be converted into a rational expression, whose numerator was `a` and denominator `2`. With `rational` on, the same input would become a rational expression with numerator `1/2*a` and denominator `1`. Thus the latter can be used in operations that require polynomial input whereas the former could not.

### 9.11.2 Real Coefficients in Polynomials

The switch `rounded` permits the use of arbitrary sized real coefficients in polynomial expressions. The actual precision of these coefficients can be set by the operator `precision`. For example, `precision(50)` sets the precision to fifty decimal digits. The default precision is system dependent and can be found by `precision(0)`. In this mode, denominators are automatically made monic, and an appropriate adjustment is made to the numerator.

*Example:* With `rounded` on, the input expression `a/2` would be converted into a rational expression whose numerator is `0.5*a` and denominator `1`.

Internally, REDUCE uses floating point numbers up to the precision supported by the underlying machine hardware, and so-called *bigfloats* for higher precision or whenever necessary to represent numbers whose value cannot be represented in floating point. The internal precision is two decimal digits greater than the external precision to guard against roundoff inaccuracies. Bigfloats represent the fraction and exponent parts of a floating-point number by means of (arbitrary precision) integers, which is a more precise representation in many cases than the machine floating point arithmetic, but not as efficient. If a case arises where use of the machine arithmetic leads to problems, a user can force REDUCE to use the bigfloat representation at all precisions by turning on the switch `roundbf`. In rare cases, this switch is turned on by the system, and the user informed by the message
```
        ROUNDBF turned on to increase accuracy
```
Rounded numbers are normally printed to the specified precision. However, if the user wishes to print such numbers with less precision, the printing precision can be set by the command `print_precision`. For example, `print_precision(5)` will cause such numbers to be printed with five digits maximum.

Under normal circumstances when `rounded` is on, REDUCE converts the number `1.0` to the integer `1`. If this is not desired, the switch `noconvert` can be turned on.

Numbers that are stored internally as bigfloats are normally printed with a space between every five digits to improve readability. If this feature is not required, it can be suppressed by turning off the switch `bfspace`.

Further information on the bigfloat arithmetic may be found in T. Sasaki, “Manual for Arbitrary Precision Real Arithmetic System in REDUCE”, Department of Computer Science, University of Utah, Technical Note No. TR-8 (1979).

When a real number is input, it is normally truncated to the precision in effect at the time the number is read. If it is desired to keep the full precision of all numbers input, the switch `adjprec` (for adjust precision) can be turned on. While on, `adjprec` will automatically increase the precision, when necessary, to match that of any integer or real input, and a message printed to inform the user of the precision increase.

When `rounded` is on, rational numbers are normally converted to rounded representation. However, if a user wishes to keep such numbers in a rational form until used in an operation that returns a real number, the switch `roundall` can be turned off. This switch is normally on.

Results from rounded calculations are returned in rounded form with two exceptions: if the result is recognized as `0` or `1` to the current precision, the integer result is returned.

### 9.11.3 Modular Number Coefficients in Polynomials

```@docs
Reduce.Algebra.setmod
```

Modular numbers are by default represented by integers in the interval ``[0,p-1]`` where ``p`` is the current modulus. Sometimes it is more convenient to use an equivalent symmetric representation in the interval ``[-p/2+1,p/2]``, or more precisely ``[-floor((p-1)/2), ceiling((p-1)/2)]``, especially if the modular numbers map objects that include negative quantities. The switch `balanced_mod` allows you to select the symmetric representation for output.

Users should note that the modular calculations are on the polynomial coefficients only. It is not currently possible to reduce the exponents since no check for a prime modulus is made (which would allow ``x^p-1`` to be reduced to 1 mod ``p``). Note also that any division by a number not co-prime with the modulus will result in the error “Invalid modular division”.

### 9.11.4 Complex Number Coefficients in Polynomials

Although REDUCE routinely treats the square of the variable ``i`` as equivalent to -1, this is not sufficient to reduce expressions involving ``i`` to lowest terms, or to factor such expressions over the complex numbers. For example, in the default case,
```Julia
julia> Algebra.factorize(:(a^2+1))
```
gives the result
```Julia
(:(a ^ 2 + 1), 1)
```
and
```
        (a^2+b^2)/(a+i*b)
```
is not reduced further. However, if the switch `complex` is turned on, full complex arithmetic is then carried out. In other words, the above factorization will give the result
```Julia
(:(a + im, 1), (a - im, 1))
```
and the quotient will be reduced to `a-I*b`.

The switch `complex` may be combined with `rounded` to give complex real numbers; the appropriate arithmetic is performed in this case.

Complex conjugation is used to remove complex numbers from denominators of expressions. To do this if `complex` is off, you must turn the switch `rationalize` on.

## 9.12 ROOT_VAL Operator

```@docs
Reduce.Algebra.root_val
```
