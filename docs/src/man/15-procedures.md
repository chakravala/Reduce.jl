# 15 Procedures

It is often useful to name a statement for repeated use in calculations with varying parameters, or to define a complete evaluation procedure for an operator. REDUCE offers a procedural declaration for this purpose. Its general syntax is:
```
[⟨procedural type⟩] PROCEDURE ⟨name⟩[⟨varlist⟩];⟨statement⟩;
```
where
```
⟨varlist⟩ ::= (⟨variable⟩,…,⟨variable⟩)
```
This will be explained more fully in the following sections.

In the algebraic mode of REDUCE the `⟨procedural type⟩` can be omitted, since the default is `ALGEBRAIC`. Procedures of type `INTEGER` or `REAL` may also be used. In the former case, the system checks that the value of the procedure is an integer. At present, such checking is not done for a real procedure, although this will change in the future when a more complete type checking mechanism is installed. Users should therefore only use these types when appropriate. An empty variable list may also be omitted.

All user-defined procedures are automatically declared to be operators.

In order to allow users relatively easy access to the whole REDUCE source program, system procedures are not protected against user redefinition. If a procedure is redefined, a message
```
        *** <procedure name> REDEFINED
```
is printed. If this occurs, and the user is not redefining his own procedure, he is well advised to rename it, and possibly start over (because he has *already* redefined some internal procedure whose correct functioning may be required for his job!)

All required procedures should be defined at the top level, since they have global scope throughout a program. In particular, an attempt to define a procedure within a procedure will cause an error to occur.

```@contents
Pages = ["15-procedures.md"]
```

## 15.1 Procedure Heading

Each procedure has a heading consisting of the word `procedure` (optionally preceded by the word `ALGEBRAIC`), followed by the name of the procedure to be defined, and followed by its formal parameters – the symbols that will be used in the body of the definition to illustrate what is to be done. There are three cases:

1. No parameters. Simply follow the procedure name with a terminator (semicolon or dollar sign).
```
        procedure abc;
```
When such a procedure is used in an expression or command, abc(), with empty parentheses, must be written.
2. One parameter. Enclose it in parentheses or just leave at least one space, then follow with a terminator.
```
        procedure abc(x);
```
or
```
        procedure abc x;
```
3. More than one parameter. Enclose them in parentheses, separated by commas, then follow with a terminator.
```
        procedure abc(x,y,z);
```

Referring to the last example, if later in some expression being evaluated the symbols `abc(u,p*q,123)` appear, the operations of the procedure body will be carried out as if `x` had the same value as `u` does, `y` the same value as `p*q` does, and `z` the value `123`. The values of `x`, `y`, `z`, after the procedure body operations are completed are unchanged. So, normally, are the values of `u`, `p`, `q`, and (of course) `123`. (This is technically referred to as call by value.)

The reader will have noted the word *normally* a few lines earlier. The call by value protections can be bypassed if necessary, as described elsewhere.

## 15.2 Procedure Body

Following the delimiter that ends the procedure heading must be a *single* statement defining the action to be performed or the value to be delivered. A terminator must follow the statement. If it is a semicolon, the name of the procedure just defined is printed. It is not printed if a dollar sign is used.

If the result wanted is given by a formula of some kind, the body is just that formula, using the variables in the procedure heading.

*Simple Example:* If `f(x)` is to mean `(x+5)*(x+6)/(x+7)`, the entire procedure definition could read
```
        procedure f x; (x+5)*(x+6)/(x+7);
```
Then `f(10)` would evaluate to `240/17`, `f(a-6)` to `a*(a-1)/(a+1)`, and so on.

*More Complicated Example:* Suppose we need a function `p(n,x)` that, for any positive integer `n`, is the Legendre polynomial of order `n`. We can define this operator using the textbook formula defining these functions:
``
p_n(x) = \frac{1}{n!} \frac{d^n}{dy^n} \frac{1}{(y^2-2xy+1)^{\frac{1}{2}}} \bigg|_{y=0}
``
Put into words, the Legendre polynomial ``p_n(x)`` is the result of substituting ``y = 0`` in the ``n^{th}`` partial derivative with respect to ``y`` of a certain fraction involving ``x`` and ``y``, then dividing that by ``n!``.
This verbal formula can easily be written in REDUCE:
```
        procedure p(n,x);  
           sub(y=0,df(1/(y^2-2*x*y+1)^(1/2),y,n))  
               /(for i:=1:n product i);
```
Having input this definition, the expression evaluation
```
        2p(2,w);
```
would result in the output
```
           2  
        3*W  - 1 .
```
If the desired process is best described as a series of steps, then a group or compound statement can be used.

*Example:* The above Legendre polynomial example can be rewritten as a series of steps instead of a single formula as follows:
```
        procedure p(n,x);  
          begin scalar seed,deriv,top,fact;  
               seed:=1/(y^2 - 2*x*y +1)^(1/2);  
               deriv:=df(seed,y,n);  
               top:=sub(y=0,deriv);  
               fact:=for i:=1:n product i;  
               return top/fact  
          end;
```
Procedures may also be defined recursively. In other words, the procedure body can include references to the procedure name itself, or to other procedures that themselves reference the given procedure. As an example, we can define the Legendre polynomial through its standard recurrence relation:
```
        procedure p(n,x);  
           if n<0 then rederr ~Invalid argument to P(N,X)~  
            else if n=0 then 1  
            else if n=1 then x  
            else ((2*n-1)*x*p(n-1,x)-(n-1)*p(n-2,x))/n;
```
The operator `rederr` in the above example provides for a simple error exit from an algebraic procedure (and also a block). It can take a string as argument.

It should be noted however that all the above definitions of `p(n,x)` are quite inefficient if extensive use is to be made of such polynomials, since each call effectively recomputes all lower order polynomials. It would be better to store these expressions in an array, and then use say the recurrence relation to compute only those polynomials that have not already been derived. We leave it as an exercise for the reader to write such a definition.

## 15.3 Matrix-valued Procedures

Normally, procedures can only return scalar values. In order for a procedure to return a matrix, it has to be declared of type `matrixproc`:
```
        matrixproc SkewSym1 (w);  
           mat((0,-w(3,1),w(2,1)),  
               (w(3,1),0,-w(1,1)),  
               (-w(2,1), w(1,1), 0));
```
Following this declaration, the call to `SkewSym1` can be used as a matrix, e.g.
```
        X := SkewSym1(mat((qx),(qy),(qz)));  
 
 
             [  0     - qz   qy  ]  
             [                   ]  
        x := [ qz      0     - qx]  
             [                   ]  
             [ - qy   qx      0  ]  
 
        X * mat((rx),(ry),(rz));  
 
 
        [ qy*rz - qz*ry  ]  
        [                ]  
        [ - qx*rz + qz*rx]  
        [                ]  
        [ qx*ry - qy*rx  ]
```

## 15.4 Using LET Inside Procedures

By using `let` instead of an assignment in the procedure body it is possible to bypass the call-by-value protection. If `x` is a formal parameter or local variable of the procedure (i.e. is in the heading or in a local declaration), and `let` is used instead of `:=` to make an assignment to `x`, e.g.
```
        let x = 123;
```
then it is the variable that is the value of `x` that is changed. This effect also occurs with local variables defined in a block. If the value of `x` is not a variable, but a more general expression, then it is that expression that is used on the left-hand side of the `let` statement. For example, if `x` had the value `p*q`, it is as if `let p*q = 123` had been executed.

## 15.5 LET Rules as Procedures

The `let` statement offers an alternative syntax and semantics for procedure definition.

In place of
```
        procedure abc(x,y,z); <procedure body>;
```
one can write
```
        for all x,y,z let abc(x,y,z) = <procedure body>;
```
There are several differences to note.

If the procedure body contains an assignment to one of the formal parameters, e.g.
```
        x := 123;
```
in the `procedure` case it is a variable holding a copy of the first actual argument that is changed. The actual argument is not changed.

In the `let` case, the actual argument is changed. Thus, if `abc` is defined using `let`, and `abc(u,v,w)` is evaluated, the value of `u` changes to `123`. That is, the `let` form of definition allows the user to bypass the protections that are enforced by the call by value conventions of standard `procedure` definitions.

*Example:* We take our earlier `factorial` procedure and write it as a `let` statement.
```
        for all n let factorial n =  
                    begin scalar m,s;  
                    m:=1; s:=n;  
                l1: if s=0 then return m;  
                    m:=m*s;  
                    s:=s-1;  
                    go to l1  
                end;
```
The reader will notice that we introduced a new local variable, `s`, and set it equal to `n`. The original form of the procedure contained the statement `n:=n-1;`. If the user asked for the value of `factorial(5)` then `n` would correspond to, not just have the value of, `5`, and REDUCE would object to trying to execute the statement `5 := 5 - 1`.

If `pqr` is a procedure with no parameters,
```
        procedure pqr;  
           <procedure body>;
```
it can be written as a `let` statement quite simply:
```
        let pqr = <procedure body>;
```
To call procedure `pqr`, if defined in the latter form, the empty parentheses would not be used: use `pqr` not `pqr()` where a call on the procedure is needed.

The two notations for a procedure with no arguments can be combined. `pqr` can be defined in the standard `procedure` form. Then a `let` statement
```
        let pqr = pqr();
```
would allow a user to use `pqr` instead of `pqr()` in calling the procedure.

A feature available with `let`-defined procedures and not with procedures defined in the standard way is the possibility of defining partial functions.
```
    for all x such that numberp x let uvw(x)=<procedure body>;
```
Now `uvw` of an integer would be calculated as prescribed by the procedure body, while `uvw` of a general argument, such as `z` or `p+q` (assuming these evaluate to themselves) would simply stay `uvw(z)` or `uvw(p+q)` as the case may be.

## 15.6 REMEMBER Statement

Setting the remember option for an algebraic procedure by
```
     REMEMBER (PROCNAME:procedure);
```
saves all intermediate results of such procedure evaluations, including recursive calls. Subsequent calls to the procedure can then be determined from the saved results, and thus the number of evaluations (or the complexity) can be reduced. This mode of evalation costs extra memory, of course. In addition, the procedure must be free of side–effects.

The following examples show the effect of the remember statement on two well–known examples.
```
procedure H(n);      % Hofstadter’s function  
 if numberp n then  
 << cnn := cnn +1;   % counts the calls  
 if n < 3 then 1 else H(n-H(n-1))+H(n-H(n-2))>>;  
 
remember h;  
 
<< cnn := 0; H(100); cnn>>;  
 
100  
 
% H has been called 100 times only.  
 
procedure A(m,n);    % Ackermann function  
 
 if m=0 then n+1 else  
  if n=0 then A(m-1,1) else  
  A(m-1,A(m,n-1));  
 
remember a;  
 
A(3,3);  
```
