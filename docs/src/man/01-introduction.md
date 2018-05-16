# 1 Introductory Information

REDUCE is a system for carrying out algebraic operations accurately, no matter how complicated the expressions become. It can manipulate polynomials in a variety of forms, both expanding and factoring them, and extract various parts of them as required. REDUCE can also do differentiation and integration, but we shall only show trivial examples of this in this introduction. Other topics not considered include the use of arrays, the definition of procedures and operators, the specific routines for high energy physics calculations, the use of files to eliminate repetitious typing and for saving results, and the editing of the input text.

Also not considered in any detail in this introduction are the many options that are available for varying computational procedures, output forms, number systems used, and so on.

REDUCE is designed to be an interactive system, so that the user can input an algebraic expression and see its value before moving on to the next calculation. For those systems that do not support interactive use, or for those calculations, especially long ones, for which a standard script can be defined, REDUCE can also be used in batch mode. In this case, a sequence of commands can be given to REDUCE and results obtained without any user interaction during the computation.

In this introduction, we shall limit ourselves to the interactive use of REDUCE, since this illustrates the capabilities of the system. *However, keep  in mind that Reduce.jl also provides Julia methods that automatically parse these statements from/into Julia AST.* When REDUCE is called, it begins by printing a banner message like:
```Julia
julia> using Reduce
Reduce (Free CSL version, revision 4521),  11-March-2018 ...
```
where the version number and the system release date will change from time to time in [deps/svn.jl](https://github.com/chakravala/Reduce.jl/blob/master/deps/svn.jl). To enter the Reduce REPL, type `<shift>`+`<]>` as the first character in the Julia prompt.
```Julia
reduce>
```
You can now type a REDUCE statement, terminated by a semicolon to indicate the end of the expression, for example:
```Julia
reduce> (x+y+z)^2;
```
This expression would normally be followed by another character (a `<Return>` on an ASCII keyboard) to “wake up” the system, which would then input the expression, evaluate it, and return the result:
```Julia
           2
(y + z + x)
```
**Note that in Julia the `expand` switch is disabled by default, unlike the standalone version.**

Let us review this simple example to learn a little more about the way that REDUCE works. First, we note that REDUCE deals with variables, and constants like other computer languages, but that in evaluating the former, a variable can stand for itself. Expression evaluation normally follows the rules of high school algebra, so the only surprise in the above example might be that the expression was expanded. REDUCE normally expands expressions where possible, collecting like terms and ordering the variables in a specific manner. However, expansion, ordering of variables, format of output and so on is under control of the user, and various declarations are available to manipulate these.

Another characteristic of the above example is the use of lower case on input and upper case on output. In fact, input may be in either mode, but output is usually in lower case. To make the difference between input and output more distinct in this manual, all expressions intended for input will be shown in lower case and output in upper case. However, for stylistic reasons, we represent all single identifiers in the text in upper case.

Finally, the numerical prompt can be used to reference the result in a later computation.
As a further illustration of the system features, the user should try:
```Julia
reduce> for i:= 1:40 product i;
```
The result in this case is the value of 40!,
```Julia
815915283247897734345611269596115894272000000000
```
You can also get the same result by saying
```Julia
reduce> factorial 40;
```
Since we want exact results in algebraic calculations, it is essential that integer arithmetic be performed to arbitrary precision, as in the above example. Furthermore, the `for` statement in the above is illustrative of a whole range of combining forms that REDUCE supports for the convenience of the user.

Among the many options in REDUCE is the use of other number systems, such as multiple precision floating point with any specified number of digits — of use if roundoff in, say, the 100th digit is all that can be tolerated.

In many cases, it is necessary to use the results of one calculation in succeeding calculations. One way to do this is via an assignment for a variable, such as
```Julia
reduce> u := (x+y+z)^2;
```
If we now use `u` in later calculations, the value of the right-hand side of the above will be used.

The results of a given calculation are also saved in the variable `ws` (for WorkSpace), so this can be used in the next calculation for further processing.

For example, the expression
```Julia
reduce> df(ws,x);
```
following the previous evaluation will calculate the derivative of `(x+y+z)^2` with respect to `x`. Alternatively,
```Julia
reduce> int(ws,y);
```
would calculate the integral of the same expression with respect to `y`.
REDUCE is also capable of handling symbolic matrices. For example,
```Julia
reduce> matrix m(2,2);
```
declares `m` to be a two by two matrix, and
```Julia
reduce> m := mat((a,b),(c,d));
```
gives its elements values. Expressions that include `m` and make algebraic sense may now be evaluated, such as `1/m` to give the inverse, `2*m - u*m^2` to give us another matrix and `det(m)` to give us the determinant of `m`.

REDUCE has a wide range of substitution capabilities. The system knows about elementary functions, but does not automatically invoke many of their well-known properties. For example, products of trigonometrical functions are not converted automatically into multiple angle expressions, but if the user wants this, he can say, for example:
```Julia
reduce> (sin(a+b)+cos(a+b))*(sin(a-b)-cos(a-b))  
            where cos(~x)*cos(~y) => (cos(x+y)+cos(x-y))/2,  
                  cos(~x)*sin(~y) => (sin(x+y)-sin(x-y))/2,  
                  sin(~x)*sin(~y) => (cos(x-y)-cos(x+y))/2;
```
where the tilde in front of the variables `x` and `y` indicates that the rules apply for all values of those variables. The result of this calculation is
```Julia
 - (cos(2*a) + sin(2*b))
```
See also the user-contributed packages ASSIST (chapter 16.5), CAMAL (chapter 16.10) and TRIGSIMP (chapter 16.72).

Another very commonly used capability of the system, and an illustration of one of the many output modes of REDUCE, is the ability to output results in a FORTRAN compatible form. Such results can then be used in a FORTRAN based numerical calculation. This is particularly useful as a way of generating algebraic formulas to be used as the basis of extensive numerical calculations.

For example, the statements
```Julia
reduce> on fort;  
reduce> df(log(x)*(sin(x)+cos(x))/sqrt(x),x,2);
```
will result in the output
```Julia
      ans=(-(4.0*cos(x)*log(x)*x**2+4.0*cos(x)*log(x)*x-3.0*cos(x)*
     . log(x)-8.0*cos(x)*x+8.0*cos(x)+4.0*log(x)*sin(x)*x**2-4.0*log(
     . x)*sin(x)*x-3.0*log(x)*sin(x)+8.0*sin(x)*x+8.0*sin(x)))/(4.0*
     . sqrt(x)*x**2)
```
These algebraic manipulations illustrate the algebraic mode of REDUCE. REDUCE is based on Standard Lisp. A symbolic mode is also available for executing Lisp statements. These statements follow the syntax of Lisp, e.g.
```Julia
reduce> symbolic car ’(a);
```
Communication between the two modes is possible.

With this simple introduction, you are now in a position to study the material in the full REDUCE manual in order to learn just how extensive the range of facilities really is. If further tutorial material is desired, the seven REDUCE Interactive Lessons by David R. Stoutemyer are recommended. These are normally distributed with the system.
