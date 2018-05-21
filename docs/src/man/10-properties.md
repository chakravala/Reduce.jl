# 10 Assigning and Testing Algebraic Properties

Sometimes algebraic expressions can be further simplified if there is additional information about the value ranges of its components. The following section describes how to inform REDUCE of such assumptions.

```@contents
Pages = ["10-properties.md"]
```

## 10.1 REALVALUED Declaration and Check

```@docs
Reduce.Algebra.realvalued
Reduce.Algebra.notrealvalued
```

The boolean operator `realvaluedp` allows you to check if a variable, an operator, or an operator expression is known as real valued. Thus,
```Julia
julia> Algebra.realvalued(:x)

julia> R"write if realvaluedp(sin x) then ~yes~ else ~no~"

julia> R"write if realvaluedp(sin z) then ~yes~ else ~no~"
```
would print first yes and then no. For general expressions test the impart for checking the value range:
```Julia
julia> Alebra.realvalued(:x,:y); R"w:=(x+i*y); w1:=conj w" |> rcall

julia> Algebra.impart(:(w*w1))
0  

julia> Algebra.impart(:(w*w))
:(2x*y)
```

## 10.2 Declaring Expressions Positive or Negative

Detailed knowlege about the sign of expressions allows REDUCE to simplify expressions involving exponentials or `abs`. You can express assumptions about the positivity or negativity of expressions by rules for the operator `sign`. Examples:
```Julia
julia> Algebra.abs(:(a*b*c))
:(abs(a*b*c))

julia> Algebra.rlet((:(sign(a))=>1,:(sign(b))=>1)); :(abs(a*b*c) |> rcall
:(abs(c) * a * b)
 
julia> Algebra.on(:precise); Algebra.sqrt(:(x^2-2x+1))
:(abs(x - 1))

reduce> ws where sign(x-1)=>1;

x - 1
```
Here factors with known sign are factored out of an `abs` expression.
```Julia
julia> Algebra.on(:precise); Algebra.on(:factor)
 
reduce> (q*x-2q)^w;  

           w
((x - 2)*q) 

reduce> ws where sign(x-2)=>1;

 w        w  
q *(x - 2)  
```
In this case the factor ``(x - 2)^w`` may be extracted from the base of the exponential because it is known to be positive.

Note that REDUCE knows a lot about sign propagation. For example, with ``x`` and ``y`` also ``x + y``, ``x + y + π`` and ``(x + e)∕y^2`` are known as positive. Nevertheless, it is often necessary to declare additionally the sign of a combined expression. E.g. at present a positivity declaration of ``x- 2`` does not automatically lead to sign evaluation for ``x- 1`` or for ``x``.
