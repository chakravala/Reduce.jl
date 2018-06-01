# 18 Calculations in High Energy Physics

A set of REDUCE commands is provided for users interested in symbolic calculations in high energy physics. Several extensions to our basic syntax are necessary, however, to allow for the different data structures encountered.

```@contents
Pages = ["18-physics.md"]
```

## 18.1 High Energy Physics Operators

We begin by introducing three new operators required in these calculations.

### 18.1.1 . (Cons) Operator

Syntax:
```
        (EXPRN1:vector_expression)  
                 . (EXPRN2:vector_expression):algebraic.
```
The binary `.` operator, which is normally used to denote the addition of an element to the front of a list, can also be used in algebraic mode to denote the scalar product of two Lorentz four-vectors. For this to happen, the second argument must be recognizable as a vector expression at the time of evaluation. With this meaning, this operator is often referred to as the *dot* operator. In the present system, the index handling routines all assume that Lorentz four-vectors are used, but these routines could be rewritten to handle other cases.

Components of vectors can be represented by including representations of unit vectors in the system. Thus if `eo` represents the unit vector `(1,0,0,0)`, `(p.eo)` represents the zeroth component of the four-vector `p`. Our metric and notation follows Bjorken and Drell “Relativistic Quantum Mechanics” (McGraw-Hill, New York, 1965). Similarly, an arbitrary component `p` may be represented by `(p.u)`. If contraction over components of vectors is required, then the declaration `index` must be used. Thus
```
Algebra.index(:u)
```
declares `u` as an index, and the simplification of
```
        p.u * q.u
```
would result in
```
        P.Q
```
The metric tensor ``g^{μν}`` may be represented by `(u.v)`. If contraction over `u` and `v` is required, then they should be declared as indices.

Errors occur if indices are not properly matched in expressions.

If a user later wishes to remove the index property from specific vectors, he can do it with the declaration `remind`. Thus `Algebra.remind(v1,…,vn)` removes the index flags from the variables `v1` through `vn`. However, these variables remain vectors in the system.

### 18.1.2 G Operator for Gamma Matrices

```@docs
Reduce.Algebra.g
```

Thus
```
        g(l1,p) * g(l2,q)
```
denotes the product of `γ.p` associated with a fermion line identified as `l1`, and `γ.q` associated with another line identified as `l2` and where `p` and `q` are Lorentz four-vectors. A product of γ matrices associated with the same line may be written in a contracted form.

Thus
```
        g(l1,p1,p2,...,p3) = g(l1,p1)*g(l1,p2)*...*g(l1,p3) .
```
The vector `a` is reserved in arguments of `g` to denote the special ``γ`` matrix ``γ^5``. Thus
```
g(l,a)	= γ ^ 5	associated with the line L
 	 	 
g(l,p,a)	= γ ⋅ p × γ ^ 5	associated with the line L.
```
``γ^μ`` (associated with the line `L`) may be written as `g(l,u)`, with `u` flagged as an index if contraction over `u` is required.

The notation of Bjorken and Drell is assumed in all operations involving γ matrices.

### 18.1.3 EPS Operator

```@docs
Reduce.Algebra.eps
```

## 18.2 Vector Variables

Apart from the line identification identifier in the `g` operator, all other arguments of the operators in this section are vectors. Variables used as such must be declared so by the type declaration `vector`, for example:
```Julia
Algebra.vector(:p1,:p2)
```
declares `p1` and `p2` to be vectors. Variables declared as indices or given a mass are automatically declared vector by these declarations.

## 18.3 Additional Expression Types

Two additional expression types are necessary for high energy calculations, namely

### 18.3.1 Vector Expressions

These follow the normal rules of vector combination. Thus the product of a scalar or numerical expression and a vector expression is a vector, as are the sum and difference of vector expressions. If these rules are not followed, error messages are printed. Furthermore, if the system finds an undeclared variable where it expects a vector variable, it will ask the user in interactive mode whether to make that variable a vector or not. In batch mode, the declaration will be made automatically and the user informed of this by a message.

*Examples:* Assuming `p` and `q` have been declared vectors, the following are vector expressions
```
        p  
        2*q/3  
        2*x*y*p - p.q*q/(3*q.q)
```
whereas `p*q` and `p/q` are not.

### 18.3.2 Dirac Expressions

These denote those expressions which involve γ matrices. A γ matrix is implicitly a 4 × 4 matrix, and so the product, sum and difference of such expressions, or the product of a scalar and Dirac expression is again a Dirac expression. There are no Dirac variables in the system, so whenever a scalar variable appears in a Dirac expression without an associated γ matrix expression, an implicit unit 4 by 4 matrix is assumed. For example, `g(l,p) + m` denotes `g(l,p) + m*⟨unit 4 by 4 matrix⟩`. Multiplication of Dirac expressions, as for matrix expressions, is of course non-commutative.

## 18.4 Trace Calculations

When a Dirac expression is evaluated, the system computes one quarter of the trace of each γ matrix product in the expansion of the expression. One quarter of each trace is taken in order to avoid confusion between the trace of the scalar `m`, say, and `m` representing `m * ⟨unit 4 by 4 matrix⟩`. Contraction over indices occurring in such expressions is also performed. If an unmatched index is found in such an expression, an error occurs.

The algorithms used for trace calculations are the best available at the time this system was produced. For example, in addition to the algorithm developed by Chisholm for contracting indices in products of traces, REDUCE uses the elegant algorithm of Kahane for contracting indices in γ matrix products. These algorithms are described in Chisholm, J. S. R., Il Nuovo Cimento X, 30, 426 (1963) and Kahane, J., Journal Math. Phys. 9, 1732 (1968).

It is possible to prevent the trace calculation over any line identifier by the declaration `nospur`. For example,
```Julia
Algebra.nospur(:l1,:l2)
```
will mean that no traces are taken of γ matrix terms involving the line numbers `l1` and `l2`. However, in some calculations involving more than one line, a catastrophic error
```
        This NOSPUR option not implemented
```
can occur (for the reason stated!) If you encounter this error, please let us know!

A trace of a γ matrix expression involving a line identifier which has been declared `nospur` may be later taken by making the declaration `spur`.

See also the CVIT package for an alternative mechanism (chapter 16.17).

## 18.5 Mass Declarations

It is often necessary to put a particle “on the mass shell” in a calculation. This can, of course, be accomplished with a `let` command such as
```
        let p.p= m^2;
```
but an alternative method is provided by two commands `mass` and `mshell`. `mass` takes a list of equations of the form:
```
⟨vector variable⟩=⟨scalar variable⟩
```
for example,
```Julia
Algebra.mass(:(p1==m), :(q1==mu))
```
The only effect of this command is to associate the relevant scalar variable as a mass with the corresponding vector. If we now say
```
mshell ⟨vector variable⟩,…,⟨vector variable⟩⟨terminator⟩
```
and a mass has been associated with these arguments, a substitution of the form
```
⟨vector variable⟩.⟨vector variable⟩ = ⟨mass⟩^2
```
is set up. An error results if the variable has no preassigned mass.

## 18.6 Example

Not initially supported by Reduce.jl parser, see [upstream docs](http://www.reduce-algebra.com/manual/manualse185.html) for more information.

## 18.7 Extensions to More Than Four Dimensions

In our discussion so far, we have assumed that we are working in the normal four dimensions of QED calculations. However, in most cases, the programs will also work in an arbitrary number of dimensions. The command
```
vecdim ⟨expression⟩⟨terminator⟩
```
sets the appropriate dimension. The dimension can be symbolic as well as numerical. Users should note however, that the `eps` operator and the ``γ_5`` symbol (`a`) are not properly defined in other than four dimensions and will lead to an error if used.
