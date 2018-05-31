# 8 Display and Structuring of Expressions

In this section, we consider a variety of commands and operators that permit the user to obtain various parts of algebraic expressions and also display their structure in a variety of forms. Also presented are some additional concepts in the REDUCE design that help the user gain a better understanding of the structure of the system.

```@contents
Pages = ["08-display.md"]
```

## 8.1 Kernels

REDUCE is designed so that each operator in the system has an evaluation (or simplification) function associated with it that transforms the expression into an internal canonical form. This form, which bears little resemblance to the original expression, is described in detail in Hearn, A. C., “REDUCE 2: A System and Language for Algebraic Manipulation,” Proc. of the Second Symposium on Symbolic and Algebraic Manipulation, ACM, New York (1971) 128-133.

The evaluation function may transform its arguments in one of two alternative ways. First, it may convert the expression into other operators in the system, leaving no functions of the original operator for further manipulation. This is in a sense true of the evaluation functions associated with the operators `+`, `*` and `/` , for example, because the canonical form does not include these operators explicitly. It is also true of an operator such as the determinant operator `det` because the relevant evaluation function calculates the appropriate determinant, and the operator `det` no longer appears. On the other hand, the evaluation process may leave some residual functions of the relevant operator. For example, with the operator `cos`, a residual expression like `cos(x)` may remain after evaluation unless a rule for the reduction of cosines into exponentials, for example, were introduced. These residual functions of an operator are termed kernels and are stored uniquely like variables. Subsequently, the kernel is carried through the calculation as a variable unless transformations are introduced for the operator at a later stage.

In those cases where the evaluation process leaves an operator expression with non-trivial arguments, the form of the argument can vary depending on the state of the system at the point of evaluation. Such arguments are normally produced in expanded form with no terms factored or grouped in any way. For example, the expression `cos(2*x+2*y)` will normally be returned in the same form. If the argument `2*x+2*y` were evaluated at the top level, however, it would be printed as `2*(x+y)`. If it is desirable to have the arguments themselves in a similar form, the switch `intstr` (for “internal structure”), if on, will cause this to happen.
In cases where the arguments of the kernel operators may be reordered, the system puts them in a canonical order, based on an internal intrinsic ordering of the variables. However, some commands allow arguments in the form of kernels, and the user has no way of telling what internal order the system will assign to these arguments. To resolve this difficulty, we introduce the notion of a kernel form as an expression that transforms to a kernel on evaluation.

Examples of kernel forms are:
```Julia
R"a"
R"cos(x*y)"
R"log(sin(x))"
```
whereas
```Julia
R"a*b"
R"(a+b)^4"
```
are not.

We see that kernel forms can usually be used as generalized variables, and most algebraic properties associated with variables may also be associated with kernels.

## 8.2 The Expression Workspace

```@docs
Reduce.Algebra.ws
Reduce.Algebra.saveas
```

A further method for referencing more than the last expression is described in chapter 13 on interactive use of REDUCE.

## 8.3 Output of Expressions

A considerable degree of flexibility is available in REDUCE in the printing of expressions generated during calculations. No explicit format statements are supplied, as these are in most cases of little use in algebraic calculations, where the size of output or its composition is not generally known in advance. Instead, REDUCE provides a series of mode options to the user that should enable him to produce his output in a comprehensible and possibly pleasing form.

The most extreme option offered is to suppress the output entirely from any top level evaluation. This is accomplished by turning off the switch `output` which is normally on. It is useful for limiting output when loading large files or producing “clean” output from the prettyprint programs.

In most circumstances, however, we wish to view the output, so we need to know how to format it appropriately. As we mentioned earlier, an algebraic expression is normally printed in an expanded form, filling the whole output line with terms. Certain output declarations, however, can be used to affect this format. To begin with, we look at an operator for changing the length of the output line.

### 8.3.1 LINELENGTH Operator

```@docs
Reduce.linelength
```

### 8.3.2 Output Declarations

We now describe a number of switches and declarations that are available for controlling output formats. It should be noted, however, that the transformation of large expressions to produce these varied output formats can take a lot of computing time and space. If a user wishes to speed up the printing of the output in such cases, he can turn off the switch `pri`. If this is done, then output is produced in one fixed format, which basically reflects the internal form of the expression, and none of the options below apply. `pri` is normally on.

With `pri` on, the output declarations and switches available are as follows:

#### ORDER Declaration

```@docs
Reduce.Algebra.order
```

#### FACTOR Declaration

```@docs
Reduce.Algebra.factor
```

Note that `factor` does not affect the order of its arguments. You should also use `order` if this is important.

```@docs
Reduce.Algebra.remfac
```

### 8.3.3 Output Control Switches

In addition to these declarations, the form of the output can be modified by switching various output control switches using the declarations `on` and `off`. We shall illustrate the use of these switches by an example, namely the printing of the expression
```Julia
R"x^2*(y^2+2*y)+x*(y^2+z)/(2*a)"
```
The relevant switches are as follows:

#### ALLFAC Switch

This switch will cause the system to search the whole expression, or any sub-expression enclosed in parentheses, for simple multiplicative factors and print them outside the parentheses. Thus our expression with `allfac` off will print as
```
    2  2        2          2  
(2*x *y *a + 4*x *y*a + x*y  + x*z)/(2*a)
```
and with `allfac` on as
```
        2                2  
x*(2*x*y *a + 4*x*y*a + y  + z)/(2*a)
```
`allfac` is normally on, and is on in the following examples, except where otherwise stated.

#### DIV Switch

This switch makes the system search the denominator of an expression for simple factors that it divides into the numerator, so that rational fractions and negative powers appear in the output. With `div` on, our expression would print as
```
      2                2  (-1)        (-1)  
x*(x*y  + 2*x*y + 1/2*y *a     + 1/2*a    *z)
```
`div` is normally off.

#### LIST Switch

This switch causes the system to print each term in any sum on a separate line. With `list` on, our expression prints as
```
        2  
x*(2*x*y *a  

    + 4*x*y*a  

       2  
    + y  

    + z)/(2*a)
```
`list` is normally off.

#### NOSPLIT Switch

Under normal circumstances, the printing routines try to break an expression across lines at a natural point. This is a fairly expensive process. If you are not overly concerned about where the end-of-line breaks come, you can speed up the printing of expressions by turning off the switch `nosplit`. This switch is normally on.

#### RAT Switch

This switch is only useful with expressions in which variables are factored with `factor`. With this mode, the overall denominator of the expression is printed with each factored sub-expression. We assume a prior declaration `factor(:x)` in the following output. We first print the expression with `rat` set to off:
```
    2                   2  
(2*x *y*a*(y + 2) + x*(y  + z))/(2*a)
```
With `rat` on the output becomes:
```
 2                 2  
x *y*(y + 2) + x*(y  + z)/(2*a)
```
`rat` is normally off.

Next, if we leave `x` factored, and turn on both `div` and `rat`, the result becomes
```
 2                    (-1)   2  
x *y*(y + 2) + 1/2*x*a    *(y  + z)
```
Finally, with `x` factored, `rat` on and `allfac` off we retrieve the original structure
```
 2   2              2  
x *(y  + 2*y) + x*(y  + z)/(2*a)
```

#### RATPRI Switch

If the numerator and denominator of an expression can each be printed in one line, the output routines will print them in a two dimensional notation, with numerator and denominator on separate lines and a line of dashes in between. For example, `(a+b)/2` will print as
```
A + B  
-----  
  2
```
Turning this switch off causes such expressions to be output in a linear form.

#### REVPRI Switch

The normal ordering of terms in output is from highest to lowest power. In some situations (e.g., when a power series is output), the opposite ordering is more convenient. The switch `revpri` if on causes such a reverse ordering of terms. For example, the expression `y*(x+1)^2+(y+3)^2` will normally print as
```
 2              2  
X *Y + 2*X*Y + Y  + 7*Y + 9
```
whereas with `revpri` on, it will print as
```
           2            2  
9 + 7*Y + Y  + 2*X*Y + X *Y
```

### 8.3.4 WRITE Command

In simple cases no explicit output command is necessary in REDUCE, since the value of any expression is automatically printed if a semicolon is used as a delimiter. There are, however, several situations in which such a command is useful.

In a `for`, `while`, or `repeat` statement it may be desired to output something each time the statement within the loop construct is repeated.

It may be desired for a procedure to output intermediate results or other information while it is running. It may be desired to have results labeled in special ways, especially if the output is directed to a file or device other than the terminal.

The `write` command consists of the word `write` followed by one or more items separated by commas, and followed by a terminator. There are three kinds of items that can be used:

1. Expressions (including variables and constants). The expression is evaluated, and the result is printed out.
2. Assignments. The expression on the right side of the `:=` operator is evaluated, and is assigned to the variable on the left; then the symbol on the left is printed, followed by a “`:=`”, followed by the value of the expression on the right – almost exactly the way an assignment followed by a semicolon prints out normally. (The difference is that if the `write` is in a `for` statement and the left-hand side of the assignment is an array position or something similar containing the variable of the `for` iteration, then the value of that variable is inserted in the printout.)
3. Arbitrary strings of characters, preceded and followed by double-quote marks (e.g., `R"~string~"`).

The items specified by a single `write` statement print side by side on one line. (The line is broken automatically if it is too long.) Strings print exactly as quoted. The `write` command itself however does not return a value.

The print line is closed at the end of a `write` command evaluation. Therefore the command `R"write ~~"` (specifying nothing to be printed except the empty string) causes a line to be skipped.

*Examples:*

1. If `a` is `x+5`, `b` is itself, `c` is `123`, `m` is an array, and `q=3`, then
```Julia
R"write m(q):=a,~ ~,b/c,~ THANK YOU~"
```
will set `m(3)` to `x+5` and print
```
m(q) := x + 5 b/123 THANK YOU
```
The blanks between the `5` and `b`, and the `3` and `t`, come from the blanks in the quoted strings.
2. To print a table of the squares of the integers from 1 to 20:
```Julia
R"for i:=1:20 do write i,~ ~,i^2"
```
3. To print a table of the squares of the integers from 1 to 20, and at the same time store them in positions 1 to 20 of an array `a`:
```Julia
R"for i:=1:20 do <<a(i):=i^2; write i,~ ~,a(i)>>"
```
This will give us two columns of numbers. If we had used
```Julia
R"for i:=1:20 do write i,~ ~,a(i):=i^2"
```
we would also get `a(i) :=` repeated on each line.
4. The following more complete example calculates the famous ``f`` and ``g`` series, first reported in Sconzo, P., LeSchack, A. R., and Tobey, R., “Symbolic Computation of f and g Series by Computer”, Astronomical Journal 70 (May 1965).
```
 x1:= -sig*(mu+2*eps)$  
 x2:= eps - 2*sig^2$  
 x3:= -3*mu*sig$  
 f:= 1$  
 g:= 0$  
 for i:= 1 step 1 until 10 do begin  
    f1:= -mu*g+x1*df(f,eps)+x2*df(f,sig)+x3*df(f,mu);  
    write ~f(~,i,~) := ~,f1;  
    g1:= f+x1*df(g,eps)+x2*df(g,sig)+x3*df(g,mu);  
    write ~g(~,i,~) := ~,g1;  
    f:=f1$  
    g:=g1$  
   end;
```
A portion of the output, to illustrate the printout from the `write` command, is as follows:
```
                ... <prior output> ...  
 
                           2  
 F(4) := MU*(3*EPS - 15*SIG  + MU)  
 
 G(4) := 6*SIG*MU  
 
                                    2  
 F(5) := 15*SIG*MU*( - 3*EPS + 7*SIG  - MU)  
 
                           2  
 G(5) := MU*(9*EPS - 45*SIG  + MU)  
 
                ... <more output> ...  
```

### 8.3.5 Suppression of Zeros

It is sometimes annoying to have zero assignments (i.e. assignments of the form `<expression> := 0`) printed, especially in printing large arrays with many zero elements. The output from such assignments can be suppressed by turning on the switch `nero`.

### 8.3.6 FORTRAN Style Output Of Expressions

It is naturally possible to evaluate expressions numerically in REDUCE by giving all variables and sub-expressions numerical values. However, as we pointed out elsewhere the user must declare real arithmetical operation by turning on the switch `rounded`. However, it should be remembered that arithmetic in REDUCE is not particularly fast, since results are interpreted rather than evaluated in a compiled form. The user with a large amount of numerical computation after all necessary algebraic manipulations have been performed is therefore well advised to perform these calculations in a FORTRAN or similar system. For this purpose, REDUCE offers facilities for users to produce FORTRAN compatible files for numerical processing.

First, when the switch `fort` is on, the system will print expressions in a FORTRAN notation. Expressions begin in column seven. If an expression extends over one line, a continuation mark (.) followed by a blank appears on subsequent cards. After a certain number of lines have been produced (according to the value of the variable `card_no`), a new expression is started. If the expression printed arises from an assignment to a variable, the variable is printed as the name of the expression. Otherwise the expression is given the default name `ans`. An error occurs if identifiers or numbers are outside the bounds permitted by FORTRAN.

A second option is to use the `write` command to produce other programs.

*Example:* The following REDUCE statements
```
 on fort;  
 out ~forfil~;  
 write ~C     this is a fortran program~;  
 write ~ 1    format(e13.5)~;  
 write ~      u=1.23~;  
 write ~      v=2.17~;  
 write ~      w=5.2~;  
 x:=(u+v+w)^11;  
 write ~C     it was foolish to expand this expression~;  
 write ~      print 1,x~;  
 write ~      end~;  
 shut ~forfil~;  
 off fort;
```
will generate a file `forfil` that contains:
```
c this is a fortran program  
 1    format(e13.5)  
      u=1.23  
      v=2.17  
      w=5.2  
      ans1=1320.*u**3*v*w**7+165.*u**3*w**8+55.*u**2*v**9+495.*u  
     . **2*v**8*w+1980.*u**2*v**7*w**2+4620.*u**2*v**6*w**3+  
     . 6930.*u**2*v**5*w**4+6930.*u**2*v**4*w**5+4620.*u**2*v**3*  
     . w**6+1980.*u**2*v**2*w**7+495.*u**2*v*w**8+55.*u**2*w**9+  
     . 11.*u*v**10+110.*u*v**9*w+495.*u*v**8*w**2+1320.*u*v**7*w  
     . **3+2310.*u*v**6*w**4+2772.*u*v**5*w**5+2310.*u*v**4*w**6  
     . +1320.*u*v**3*w**7+495.*u*v**2*w**8+110.*u*v*w**9+11.*u*w  
     . **10+v**11+11.*v**10*w+55.*v**9*w**2+165.*v**8*w**3+330.*  
     . v**7*w**4+462.*v**6*w**5+462.*v**5*w**6+330.*v**4*w**7+  
     . 165.*v**3*w**8+55.*v**2*w**9+11.*v*w**10+w**11  
      x=u**11+11.*u**10*v+11.*u**10*w+55.*u**9*v**2+110.*u**9*v*  
     . w+55.*u**9*w**2+165.*u**8*v**3+495.*u**8*v**2*w+495.*u**8  
     . *v*w**2+165.*u**8*w**3+330.*u**7*v**4+1320.*u**7*v**3*w+  
     . 1980.*u**7*v**2*w**2+1320.*u**7*v*w**3+330.*u**7*w**4+462.  
     . *u**6*v**5+2310.*u**6*v**4*w+4620.*u**6*v**3*w**2+4620.*u  
     . **6*v**2*w**3+2310.*u**6*v*w**4+462.*u**6*w**5+462.*u**5*  
     . v**6+2772.*u**5*v**5*w+6930.*u**5*v**4*w**2+9240.*u**5*v  
     . **3*w**3+6930.*u**5*v**2*w**4+2772.*u**5*v*w**5+462.*u**5  
     . *w**6+330.*u**4*v**7+2310.*u**4*v**6*w+6930.*u**4*v**5*w  
     . **2+11550.*u**4*v**4*w**3+11550.*u**4*v**3*w**4+6930.*u**  
     . 4*v**2*w**5+2310.*u**4*v*w**6+330.*u**4*w**7+165.*u**3*v  
     . **8+1320.*u**3*v**7*w+4620.*u**3*v**6*w**2+9240.*u**3*v**  
     . 5*w**3+11550.*u**3*v**4*w**4+9240.*u**3*v**3*w**5+4620.*u  
     . **3*v**2*w**6+ans1  
c     it was foolish to expand this expression  
      print 1,x  
      end
```
If the arguments of a `write` statement include an expression that requires continuation records, the output will need editing, since the output routine prints the arguments of `write` sequentially, and the continuation mechanism therefore generates its auxiliary variables after the preceding expression has been printed.

Finally, since there is no direct analog of *list* in FORTRAN, a comment line of the form
```
c ***** invalid fortran construct (list) not printed
```
will be printed if you try to print a list with `fort` on.

#### FORTRAN Output Options

There are a number of methods available to change the default format of the FORTRAN output.

The breakup of the expression into subparts is such that the number of continuation lines produced is less than a given number. This number can be modified by the assignment
```Julia
R"card_no := ⟨number⟩"
```
where `⟨number⟩` is the total number of cards allowed in a statement. The default value of `card_no` is 20.

The width of the output expression is also adjustable by the assignment
```Julia
R"fort_width := ⟨integer⟩"
```
`fort_width` which sets the total width of a given line to `⟨integer⟩`. The initial FORTRAN output width is 70.

REDUCE automatically inserts a decimal point after each isolated integer coefficient in a FORTRAN expression (so that, for example, 4 becomes `4. `). To prevent this, set the `period` mode switch to `off`.

FORTRAN output is normally produced in lower case. If upper case is desired, the switch `FORTUPPER` should be turned on.

Finally, the default name `ans` assigned to an unnamed expression and its subparts can be changed by the operator `varname`. This takes a single identifier as argument, which then replaces ANS as the expression name. The value of `varname` is its argument.

Further facilities for the production of FORTRAN and other language output are provided by the SCOPE and GENTRAN packagesdescribed in chapters 16.26 and 16.60.

### 8.3.7 Saving Expressions for Later Use as Input

It is often useful to save an expression on an external file for use later as input in further calculations. The commands for opening and closing output files are explained elsewhere. However, we see in the examples on output of expressions that the standard “natural” method of printing expressions is not compatible with the input syntax. So to print the expression in an input compatible form we must inhibit this natural style by turning off the switch `nat`. If this is done, a dollar sign will also be printed at the end of the expression.

*Example:* The following sequence of commands
```
        off nat; out ~out~; x := (y+z)^2; write ~end~;  
        shut ~out~; on nat;
```
will generate a file out that contains
```
        X := Y**2 + 2*Y*Z + Z**2$  
        END$
```

### 8.3.8 Displaying Expression Structure

In those cases where the final result has a complicated form, it is often convenient to display the skeletal structure of the answer. The operator `structr`, that takes a single expression as argument, will do this for you. Its syntax is:
```Julia
R"structr(EXPRN:algebraic[,ID1:identifier[,ID2:identifier]])"
```
The structure is printed effectively as a tree, in which the subparts are laid out with auxiliary names. If the optional `ID1` is absent, the auxiliary names are prefixed by the root `ans`. This root may be changed by the operator `varname`. If the optional `ID1` is present, and is an array name, the subparts are named as elements of that array, otherwise `ID1` is used as the root prefix. (The second optional argument `ID2` is explained later.)

The `EXPRN` can be either a scalar or a matrix expression. Use of any other will result in an error.

*Example:* Let us suppose that the workspace contains `((a+b)^2+c)^3+d`. Then the input `R"structr ws" will (with `exp` off) result in the output:
```
ans3

   where

                  3
      ans3 := ans2  + d

                  2
      ans2 := ans1  + c

      ans1 := a + b
```
The workspace remains unchanged after this operation, since `structr` in the default situation returns no value (if `structr` is used as a sub-expression, its value is taken to be 0). In addition, the sub-expressions are normally only displayed and not retained. If you wish to access the sub-expressions with their displayed names, the switch `savestructr` should be turned on. In this case, `structr` returns a list whose first element is a representation for the expression, and subsequent elements are the sub-expression relations. Thus, with `savestructr` on, `R"structr ws"` in the above example would return
```
               3              2
{ans3,ans3=ans2  + d,ans2=ans1  + c,ans1=a + b}
```
The `part` operator can be used to retrieve the required parts of the expression. For example, to get the value of `ans2` in the above, one could say:
```Julia
R"part(ws,3,2)"
```
If `fort` is on, then the results are printed in the reverse order; the algorithm in fact guaranteeing that no sub-expression will be referenced before it is defined. The second optional argument `ID2` may also be used in this case to name the actual expression (or expressions in the case of a matrix argument).

*Example:* Let us suppose that `m`, a 2 by 1 matrix, contains the elements `((a+b)^2 + c)^3 + d` and `(a + b)\*(c + d)` respectively, and that V has been declared to be an array. With `exp` off and `fort` on, the statement `R"structr(2\*m,v,k);"` will result in the output
```
v(1)=a+b  
v(2)=v(1)**2+c  
v(3)=v(2)**3+d  
v(4)=c+d  
k(1,1)=2.*v(3)  
k(2,1)=2.*v(1)*v(4)
```

## 8.4 Changing the Internal Order of Variables

```@docs
Reduce.Algebra.korder
```

Unlike the `order` declaration, that has a purely cosmetic effect on the way results are printed, the use of `korder` can have a significant effect on computation time. In critical cases then, the user can experiment with the ordering of the variables used to determine the optimum set for a given problem.

## 8.5 Obtaining Parts of Algebraic Expressions

There are many occasions where it is desirable to obtain a specific part of an expression, or even change such a part to another expression. A number of operators are available in REDUCE for this purpose, and will be described in this section. In addition, operators for obtaining specific parts of polynomials and rational functions (such as a denominator) are described in another section.

### 8.5.1 COEFF Operator

Syntax:
```Julia
R"coeff(EXPRN:polynomial,VAR:kernel)"
```
`coeff` is an operator that partitions `EXPRN` into its various coefficients with respect to `VAR` and returns them as a list, with the coefficient independent of `VAR` first.

Under normal circumstances, an error results if `EXPRN` is not a polynomial in `VAR`, although the coefficients themselves can be rational as long as they do not depend on `VAR`. However, if the switch `ratarg` is on, denominators are not checked for dependence on `VAR`, and are taken to be part of the coefficients.

*Example:*
```
reduce> coeff((y^2+z)^3/z,y);
```
returns the result
```
  2  
{Z ,0,3*Z,0,3,0,1/Z}
```
whereas
```
reduce> coeff((y^2+z)^3/y,y);
```
gives an error if `ratarg` is off, and the result
```
  3        2  
{Z /Y,0,3*Z /Y,0,3*Z/Y,0,1/Y}
```
if `ratarg` is on.

The length of the result of `coeff` is the highest power of `VAR` encountered plus 1. In the above examples it is 7. In addition, the variable `high_pow` is set to the highest non-zero power found in `EXPRN` during the evaluation, and `low_pow` to the lowest non-zero power, or zero if there is a constant term. If `EXPRN` is a constant, then `high_pow` and `low_pow` are both set to zero.

### 8.5.2 COEFFN Operator

The `coeffn` operator is designed to give the user a particular coefficient of a variable in a polynomial, as opposed to `coeff` that returns all coefficients. `coeffn` is used with the syntax
```
R"coeffn(EXPRN:polynomial,VAR:kernel,N:integer)"
```
It returns the `n`th coefficient of `VAR` in the polynomial `EXPRN`.

### 8.5.3 PART Operator

Syntax:
```Julia
R"part(EXPRN:algebraic[,INTEXP:integer])"
```
This operator works on the form of the expression as printed or as it would have been printed at that point in the calculation bearing in mind all the relevant switch settings at that point. The reader therefore needs some familiarity with the way that expressions are represented in prefix form in REDUCE to use these operators effectively. Furthermore, it is assumed that `pri` is `on` at that point in the calculation. The reason for this is that with `pri` off, an expression is printed by walking the tree representing the expression internally. To save space, it is never actually transformed into the equivalent prefix expression as occurs when `pri` is on. However, the operations on polynomials described elsewhere can be equally well used in this case to obtain the relevant parts.

The evaluation proceeds recursively down the integer expression list. In other words,
```Julia
part(⟨expression⟩,⟨integer1⟩,⟨integer2⟩)
→part(part(⟨expression⟩,⟨integer1⟩),⟨integer2⟩)
```
and so on, and
```
PART(⟨expression⟩)→⟨expression⟩
```
`intexp` can be any expression that evaluates to an integer. If the integer is positive, then that term of the expression is found. If the integer is 0, the operator is returned. Finally, if the integer is negative, the counting is from the tail of the expression rather than the head.

For example, if the expression `a+b` is printed as `a+b` (i.e., the ordering of the variables is alphabetical), then
```
        part(a+b,2)  ->   b  
        part(a+b,-1) ->   b  
```
and
```  
        part(a+b,0)  ->  plus
```
An operator `arglength` is available to determine the number of arguments of the top level operator in an expression. If the expression does not contain a top level operator, then -1 is returned. For example,
```
        arglength(a+b+c) ->  3  
        arglength(f())   ->  0  
        arglength(a)     ->  -1
```

### 8.5.4 Substituting for Parts of Expressions

`part` may also be used to substitute for a given part of an expression. In this case, the `part` construct appears on the left-hand side of an assignment statement, and the expression to replace the given part on the right-hand side.

For example, with the normal settings of the REDUCE switches:
```
        xx := a+b;  
        part(xx,2) := c;   ->  A+C  
        part(c+d,0) := -;   -> C-D
```
Note that `xx` in the above is not changed by this substitution. In addition, unlike expressions such as array and matrix elements that have an instant evaluation property, the values of `part(xx,2)` and `part(c+d,0)` are also not changed.
