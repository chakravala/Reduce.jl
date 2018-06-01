# 4 Lists

A list is an object consisting of a sequence of other objects (including lists themselves), separated by commas and surrounded by braces. Examples of lists are:
```Julia
R"{a,b,c}"
R"{1,a-b,c=d}"
R"{{a},{{b,c},d},e}"
```
The empty list is represented as
```Julia
R"{}"
```

```@contents
Pages = ["04-lists.md"]
```

## 4.1 Operations on Lists

Several operators in the system return their results as lists, and a user can create new lists using braces and commas. Alternatively, one can use the operator `list` to construct a list. An important class of operations on lists are `map` and `select` operations. For details, please refer to the chapters on `map`, `select` and the `for` command. See also the documentation on the ASSIST (chapter 16.5) package.

To facilitate the use of lists, a number of operators are also available for manipulating them. `R"part(⟨list⟩,n)"` for example will return the `n`th element of a list. `length` will return the length of a list. Several operators are also defined uniquely for lists. For those familiar with them, these operators in fact mirror the operations defined for Lisp lists. These operators are as follows:

### 4.1.1 LIST

```@docs
Reduce.list
```

### 4.1.2 FIRST

This operator returns the first member of a list. An error occurs if the argument is not a list, or the list is empty.

### 4.1.3 SECOND

`second` returns the second member of a list. An error occurs if the argument is not a list or has no second element.

### 4.1.4 THIRD

This operator returns the third member of a list. An error occurs if the argument is not a list or has no third element.

### 4.1.5 REST

`rest` returns its argument with the first element removed. An error occurs if the argument is not a list, or is empty.

### 4.1.6 . (Cons) Operator

This operator adds (“conses”) an expression to the front of a list. For example:
```Julia
julia> R"a . {b,c}" == R"{a,b,c}"
true
```

### 4.1.7 APPEND

This operator appends its first argument to its second to form a new list. *Examples:*
```Julia
R"append({a,b},{c,d})"     ->     R"{a,b,c,d}"
R"append({{a,b}},{c,d})"   ->     R"{{a,b},c,d}"
```

### 4.1.8 REVERSE

The operator `reverse` returns its argument with the elements in the reverse order. It only applies to the top level list, not any lower level lists that may occur. Examples are:
```Julia
julia> Algebra.reverse(list(:a,:b,:c)) ==  R"{c,b,a}"
true

julia> R"reverse({{a,b,c},d})" == R"{d,{a,b,c}}"
true
```

### 4.1.9 List Arguments of Other Operators

If an operator other than those specifically defined for lists is given a single argument that is a list, then the result of this operation will be a list in which that operator is applied to each element of the list. For example, the result of evaluating `R"log{a,b,c}"` is the expression `R"{LOG(A),LOG(B),LOG(C)}"`.

There are two ways to inhibit this operator distribution. Firstly, the switch `listargs`, if on, will globally inhibit such distribution. Secondly, one can inhibit this distribution for a specific operator by the declaration `listargp`. For example, with the declaration `R"listargp log, log{a,b,c}"` would evaluate to `R"log({a,b,c})"`.

If an operator has more than one argument, no such distribution occurs.

### 4.1.10 Caveats and Examples

Some of the natural list operations such as *member* or *delete* are available only after loading the package ASSIST (chapter 16.5).

Please note that a non-list as second argument to `cons` (a "dotted pair" in LISP terms) is not allowed and causes an "invalid as list" error.
```
julia> R"a := 17 . 4"

***** 17 4 invalid as list
```
Also, the initialization of a scalar variable is not the empty list – one has to set list type variables explicitly, as in the following example:
```
 load_package assist;  
 
 procedure lotto (n,m);  
  begin scalar list_1_n, luckies, hit;  
     list_1_n := {};  
     luckies := {};  
     for k:=1:n do list_1_n := k . list_1_n;  
     for k:=1:m do  
       << hit := part(list_1_n,random(n-k+1) + 1);  
          list_1_n := delete(hit,list_1_n);  
          luckies := hit . luckies >>;  
     return luckies;  
  end;  
                 % In Germany, try lotto (49,6);
```
*Another example:* Find all coefficients of a multivariate polynomial with respect to a list of variables:
```
procedure allcoeffs(q,lis);  
   % q : polynomial, lis: list of vars  
   allcoeffs1 (list q,lis);  
 
procedure allcoeffs1(q,lis);  
  if lis={} then q else  
    allcoeffs1(foreach qq in q join coeff(qq,first lis),  
               rest lis);
```
