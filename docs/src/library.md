# Reduce.jl Library

```@contents
```

## Index

```@index
```

## Reduce Interface

```@docs
Reduce.Reset
```

```@docs
RExpr
```

```@docs
rcall
```

```@docs
parse
```

```@docs
load_package
```

```@docs
sub
```

```@docs
squash
```

## Imported Operators

Reduce `switch` modes callable as functions from Julia
> expand, complex, factor, horner, expandlog, combinelog, precise, combineexpt, rounded, evallhseq, nat, latex

Reduce operators with multiple arguments
> df, int, limit, sum, prod, +, -, ^, *, /, //

Unary operators
> abs, conj, factorial, floor, max, min, round, sign, acos, acosh, acot, acoth, acsc, acsch, asec, asech, asin, asinh, atan, atanh, atan2, cos, cosh, cot, coth, csc, csch, exp, hypot, log, log10, sec, sech, sin, sinh, sqrt, tan, tanh, gamma, factorize

> beta, besseli, besselj, besselk, bessely, polygamma, zeta

> ibeta, igamma, ln, psi, bernoulli, continued_fraction, ci, dilog, ei, si, airy_ai, airy_aiprime, airy_bi, airy_biprime, hanekl1, hankel2, kummerm, kummeru, lommel1, lommel2, struveh, struvel, whittakerm, whittakeru, solidharmonicy, sphericalharmonicy

> ceiling, fix, impart, repart, nextprime, euler, fibonacci, motzkin, random, random_new_seed

## Tools & Options

```@docs
Reduce.parsegen
```

```@docs
Reduce.unfoldgen
```

```@docs
Reduce.linefilter
```

```@docs
Reduce.Rational
```

```@docs
Reduce.SubCall
```

```@docs
Reduce.SubHold
```

```@docs
Reduce.SubFail
```

```@docs
Reduce.ColCheck
```

```@docs
Reduce.PrintLog
```
