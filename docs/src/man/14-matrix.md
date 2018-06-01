# 14 Matrix Calculations

A very powerful feature of REDUCE is the ease with which matrix calculations can be performed. To extend our syntax to this class of calculations we need to add another prefix operator, `mat`, and a further variable and expression type as follows:

```@contents
Pages = ["14-matrix.md"]
```

## 14.1 MAT Operator

This prefix operator is used to represent n × m matrices. `mat` has `n` arguments interpreted as rows of the matrix, each of which is a list of m expressions representing elements in that row. For example, the matrix
```Julia
julia> [:a :b :c; :d :e :f]
2×3 Array{Symbol,2}:
 :a  :b  :c
 :d  :e  :f
```
would be written as `R"mat((a,b,c),(d,e,f))"`.

Note that the single column matrix
```Julia
julia> [:x; :y]
2-element Array{Symbol,1}:
 :x
 :y
```
becomes `R"mat((x),(y))"`. The inside parentheses are required to distinguish it from the single row matrix
```Julia
julia> [:x :y]
1×2 Array{Symbol,2}:
 :x  :y
```
that would be written as `R"mat((x,y))"`.

## 14.2 Matrix Variables

An identifier may be declared a matrix variable by the declaration `matrix`. The size of the matrix may be declared explicitly in the matrix declaration, or by default in assigning such a variable to a matrix expression. For example,
```Julia
julia> Algebra.matrix(:(x(2,1)),:(y(3,4)),:z)
```
declares `x` to be a 2 x 1 (column) matrix, `y` to be a 3 x 4 matrix and `z` a matrix whose size is to be declared later.

Matrix declarations can appear anywhere in a program. Once a symbol is declared to name a matrix, it can not also be used to name an array, operator or a procedure, or used as an ordinary variable. It can however be redeclared to be a matrix, and its size may be changed at that time. Note however that matrices once declared are global in scope, and so can then be referenced anywhere in the program. In other words, a declaration within a block (or a procedure) does not limit the scope of the matrix to that block, nor does the matrix go away on exiting the block (use `clear` instead for this purpose). An element of a matrix is referred to in the expected manner; thus `x(1,1)` gives the first element of the matrix `x` defined above. References to elements of a matrix whose size has not yet been declared leads to an error. All elements of a matrix whose size is declared are initialized to 0. As a result, a matrix element has an *instant evaluation* property and cannot stand for itself. If this is required, then an operator should be used to name the matrix elements as in:
```Julia
julia> Algebra.matrix(:m); Algebra.operator(:x);  rcall("m := mat((x(1,1),x(1,2)))");
```

## 14.3 Matrix Expressions

These follow the normal rules of matrix algebra as defined by the following syntax:
```
⟨matrix expression⟩  ::=  MAT⟨matrix description⟩∣⟨matrix variable⟩∣
			⟨scalar expression⟩*⟨matrix expression⟩∣
			⟨matrix expression⟩*⟨matrix expression⟩∣
			⟨matrix expression⟩+⟨matrix expression⟩∣
			⟨matrix expression⟩^⟨integer⟩∣
			⟨matrix expression⟩/⟨matrix expression⟩
```
Sums and products of matrix expressions must be of compatible size; otherwise an error will result during their evaluation. Similarly, only square matrices may be raised to a power. A negative power is computed as the inverse of the matrix raised to the corresponding positive power. `a/b` is interpreted as `a*b^(-1)`.

*Examples:*

Assuming `x` and `y` have been declared as matrices, the following are matrix expressions
```
        y  
        y^2*x-3*y^(-2)*x  
        y + mat((1,a),(b,c))/2
```
The computation of the quotient of two matrices normally uses a two-step elimination method due to Bareiss. An alternative method using Cramer’s method is also available. This is usually less efficient than the Bareiss method unless the matrices are large and dense, although we have no solid statistics on this as yet. To use Cramer’s method instead, the switch `cramer` should be turned on.

## 14.4 Operators with Matrix Arguments

The operator `length` applied to a matrix returns a list of the number of rows and columns in the matrix. Other operators useful in matrix calculations are defined in the following subsections. Attention is also drawn to the LINALG (section 16.37) and NORMFORM (section 16.42) packages.

```@docs
Reduce.Algebra.det
```

```@docs
Reduce.Algebra.mateigen
```

```@docs
Reduce.Algebra.tp
Reduce.Algebra.trace
```

```@docs
Reduce.Algebra.cofactor
```

```@docs
Reduce.Algebra.nullspace
Reduce.Algebra.rank
```

## 14.5 Matrix Assignments

Matrix expressions may appear in the right-hand side of assignment statements. If the left-hand side of the assignment, which must be a variable, has not already been declared a matrix, it is declared by default to the size of the right-hand side. The variable is then set to the value of the right-hand side.

Such an assignment may be used very conveniently to find the solution of a set of linear equations. For example, to find the solution of the following set of equations
```
        a11*x(1) + a12*x(2) = y1  
        a21*x(1) + a22*x(2) = y2
```
we simply write
```
Algebra.:*(Algebra.inv([:a11 :a12; :a21 :a22]),[:y1,:y2])
```

## 14.6 Evaluating Matrix Elements

Once an element of a matrix has been assigned, it may be referred to in standard array element notation. Thus `y(2,1)` refers to the element in the second row and first column of the matrix `y`.
