# 11 Substitution Commands

An important class of commands in REDUCE define substitutions for variables and expressions to be made during the evaluation of expressions. Such substitutions use the prefix operator SUB, various forms of the command `let`, and rule sets.

```@contents
Pages = ["11-substitution.md"]
```

## 11.1 SUB Operator

```@docs
Reduce.Algebra.sub
```

The braces around the substitution list may also be omitted, as in:
```
                                    2              2  
     sub(x=a+y,y=y+1,x^2+y^2)   -> A  + 2*A*Y + 2*Y  + 2*Y + 1
```

## 11.2 LET Rules

Unlike substitutions introduced via `sub`, `let` rules are global in scope and stay in effect until replaced or `clear`ed.

```@docs
Reduce.Algebra.rlet
```

*CAUTION:* A recursive rule such as
```Julia
julia> Algebra.rlet( :x => :(x + 1) )
```
is erroneous, since any subsequent evaluation of `x` would lead to a non-terminating chain of substitutions:
```
      x -> x + 1 -> (x + 1) + 1 -> ((x + 1) + 1) + 1 -> ...
```
Similarly, coupled substitutions such as
```
julia> Algebra.rlet([:l => :(m + n), :n => :(l + r)])
```
would lead to the same error. As a result, if you try to evaluate an `x`, `l` or `n` defined as above, you will get an error such as
```
        X improperly defined in terms of itself
```
Array and matrix elements can appear on the left-hand side of a `let` statement. However, because of their instant evaluation property, it is the value of the element that is substituted for, rather than the element itself. E.g.,
```
        array a(5);  
        a(2) := b;  
        let a(2) = c;
```
results in `b` being substituted by `c`; the assignment for `a(2)` does not change.

Finally, if an error occurs in any equation in a `let` statement (including generalized statements involving `for all` and `such that`), the remaining rules are not evaluated.

### 11.2.1 FOR ALL … LET

If a substitution for all possible values of a given argument of an operator is required, the declaration FOR ALL may be used. The syntax of such a command is
```Julia
R"for all ⟨variable⟩,…,⟨variable⟩ ⟨LET statement⟩⟨terminator⟩"
```
e.g.,
```Julia
R"for all x,y let h(x,y) = x-y"
R"for all x let k(x,y) = x^y"
```
The first of these declarations would cause `h(a,b)` to be evaluated as `a-b`, `h(u+v,u+w)` to be `v-w`, etc. If the operator symbol `h` is used with more or fewer argument places, not two, the `let` would have no effect, and no error would result.

The second declaration would cause `k(a,y)` to be evaluated as `a^y`, but would have no effect on `k(a,z)` since the rule didn’t say `for all y…`.

Where we used `x` and `y` in the examples, any variables could have been used. This use of a variable doesn’t affect the value it may have outside the `let` statement. However, you should remember what variables you actually used. If you want to delete the rule subsequently, you must use the same variables in the `clear` command.

It is possible to use more complicated expressions as a template for a `let` statement, as explained in the section on substitutions for general expressions. In nearly all cases, the rule will be accepted, and a consistent application made by the system. However, if there is a sole constant or a sole free variable on the left-hand side of a rule (e.g., `R"let 2=3 or for all x let x=2"`), then the system is unable to handle the rule, and the error message
```
        Substitution for ... not allowed
```
will be issued. Any variable listed in the `for all` part will have its symbol preceded by an equal sign: `x` in the above example will appear as `=x`. An error will also occur if a variable in the `for all` part is not properly matched on both sides of the `let` equation.

### 11.2.2 FOR ALL … SUCH THAT … LET

If a substitution is desired for more than a single value of a variable in an operator or other expression, but not all values, a conditional form of the `for all … let` declaration can be used.

*Example:*
```Julia
R"for all x such that numberp x and x<0 let h(x)=0"
```
will cause `h(-5)` to be evaluated as 0, but `h` of a positive integer, or of an argument that is not an integer at all, would not be affected. Any boolean expression can follow the `such that` keywords.

### 11.2.3 Removing Assignments and Substitution Rules

```@docs
Reduce.Algebra.clear
```

The more general types of `let` declarations can also be deleted by using `clear`. Simply repeat the `let` rule to be deleted, using `clear` in place of `let`, and omitting the equal sign and right-hand part. The same dummy variables must be used in the `for all` part, and the boolean expression in the `such  that` part must be written the same way. (The placing of blanks doesn’t have to be identical.)

*Example:* The `let` rule
```Julia
R"for all x such that numberp x and x<0 let h(x)=0"
```
can be erased by the command
```Julia
R"for all x such that numberp x and x<0 clear h(x)"
```

### 11.2.4 Overlapping LET Rules

`clear` is not the only way to delete a `let` rule. A new `let` rule identical to the first, but with a different expression after the equal sign, replaces the first. Replacements are also made in other cases where the existing rule would be in conflict with the new rule. For example, a rule for `x^4` would replace a rule for `x^5`. The user should however be cautioned against having several `let` rules in effect that relate to the same expression. No guarantee can be given as to which rules will be applied by REDUCE or in what order. It is best to `clear` an old rule before entering a new related `let` rule.

### 11.2.5 Substitutions for General Expressions

The examples of substitutions discussed in other sections have involved very simple rules. However, the substitution mechanism used in REDUCE is very general, and can handle arbitrarily complicated rules without difficulty.

The general substitution mechanism used in REDUCE is discussed in Hearn, A. C., “REDUCE, A User-Oriented Interactive System for Algebraic Simplification,” Interactive Systems for Experimental Applied Mathematics, (edited by M. Klerer and J. Reinfelds), Academic Press, New York (1968), 79-90, and Hearn. A. C., “The Problem of Substitution,” Proc. 1968 Summer Institute on Symbolic Mathematical Computation, IBM Programming Laboratory Report FSC 69-0312 (1969). For the reasons given in these references, REDUCE does not attempt to implement a general pattern matching algorithm. However, the present system uses far more sophisticated techniques than those discussed in the above papers. It is now possible for the rules appearing in arguments of `let` to have the form
```
⟨substitution expression⟩ = ⟨expression⟩
```
where any rule to which a sensible meaning can be assigned is permitted. However, this meaning can vary according to the form of `⟨substitution expression⟩`. The semantic rules associated with the application of the substitution are completely consistent, but somewhat complicated by the pragmatic need to perform such substitutions as efficiently as possible. The following rules explain how the majority of the cases are handled.

To begin with, the `⟨substitution expression⟩` is first partly simplified by collecting like terms and putting identifiers (and kernels) in the system order. However, no substitutions are performed on any part of the expression with the exception of expressions with the *instant evaluation* property, such as array and matrix elements, whose actual values are used. It should also be noted that the system order used is not changeable by the user, even with the `korder` command. Specific cases are then handled as follows:

1. If the resulting simplified rule has a left-hand side that is an identifier, an expression with a top-level algebraic operator or a power, then the rule is added without further change to the appropriate table.
2. If the operator `*` appears at the top level of the simplified left-hand side, then any constant arguments in that expression are moved to the right-hand side of the rule. The remaining left-hand side is then added to the appropriate table. For example,
```Julia
julia> Algebra.rlet(:(2*x*y) => 3)
```
becomes
```Julia
julia> Algebra.rlet(:(x*y) => 3/2)
```
so that `x*y` is added to the product substitution table, and when this rule is applied, the expression `x*y` becomes `3/2`, but `x` or `y` by themselves are not replaced.
3. If the operators `+`, `-` or `/` appear at the top level of the simplified left-hand side, all but the first term is moved to the right-hand side of the rule. Thus the rules
```Julia
julia> Algebra.rlet(:(l+m)=>:n, :(x/2)=>:y, :(a-b)=>:c)
```
become
```Julia
julia> Algebra.rlet(:l=>:(n-m), :x=>:(2*y), :a=:(c+b))
```
One problem that can occur in this case is that if a quantified expression is moved to the right-hand side, a given free variable might no longer appear on the left-hand side, resulting in an error because of the unmatched free variable. E.g.,
```Julia
R"for all x,y let f(x)+f(y)=x*y"
```
would become
```Julia
R"for all x,y let f(x)=x*y-f(y)"
```
which no longer has `y` on both sides.

The fact that array and matrix elements are evaluated in the left-hand side of rules can lead to confusion at times. Consider for example the statements
```Julia
R"array a(5); let x+a(2)=3; let a(3)=4"
```
The left-hand side of the first rule will become `x`, and the second 0. Thus the first rule will be instantiated as a substitution for `x`, and the second will result in an error.

The order in which a list of rules is applied is not easily understandable without a detailed knowledge of the system simplification protocol. It is also possible for this order to change from release to release, as improved substitution techniques are implemented. Users should therefore assume that the order of application of rules is arbitrary, and program accordingly.

After a substitution has been made, the expression being evaluated is reexamined in case a new allowed substitution has been generated. This process is continued until no more substitutions can be made.

As mentioned elsewhere, when a substitution expression appears in a product, the substitution is made if that expression divides the product. For example, the rule
```Julia
julia> Algebra.rlet(:(a^2*c) => :(3*z))
```
would cause `a^2*c*x` to be replaced by `3*z*x` and `a^2*c^2` by `3*z*c`. If the substitution is desired only when the substitution expression appears in a product with the explicit powers supplied in the rule, the command `match` should be used instead.

For example,
```Julia
R"match a^2*c = 3*z"
```
would cause `a^2*c*x` to be replaced by `3*z*x`, but `a^2*c^2` would not be replaced. `match` can also be used with the `for all` constructions described above.

To remove substitution rules of the type discussed in this section, the `clear` command can be used, combined, if necessary, with the same `for all` clause with which the rule was defined, for example:
```Julia
R"for all x clear log(e^x),e^log(x),cos(w*t+theta(x))"
```
Note, however, that the arbitrary variable names in this case must be the same as those used in defining the substitution.

## 11.3 Rule Lists

Rule lists offer an alternative approach to defining substitutions that is different from either `sub` or `let`. In fact, they provide the best features of both, since they have all the capabilities of `let`, but the rules can also be applied locally as is possible with `sub`. In time, they will be used more and more in REDUCE. However, since they are relatively new, much of the REDUCE code you see uses the older constructs.

A rule list is a list of rules that have the syntax
```
     <expression> => <expression> (WHEN <boolean expression>)
```
For example,
```
        {cos(~x)*cos(~y) => (cos(x+y)+cos(x-y))/2,  
         cos(~n*pi)      => (-1)^n when remainder(n,2)=0}
```
The tilde preceding a variable marks that variable as free for that rule, much as a variable in a `for all` clause in a `let` statement. The first occurrence of that variable in each relevant rule must be so marked on input, otherwise inconsistent results can occur. For example, the rule list
```
        {cos(~x)*cos(~y) => (cos(x+y)+cos(x-y))/2,  
         cos(x)^2        => (1+cos(2x))/2}
```
designed to replace products of cosines, would not be correct, since the second rule would only apply to the explicit argument `x`. Later occurrences in the same rule may also be marked, but this is optional (internally, all such rules are stored with each relevant variable explicitly marked). The optional `when` clause allows constraints to be placed on the application of the rule, much as the `such that` clause in a `let` statement.

A rule list may be named, for example
```
        trig1 := {cos(~x)*cos(~y) => (cos(x+y)+cos(x-y))/2,  
                  cos(~x)*sin(~y) => (sin(x+y)-sin(x-y))/2,  
                  sin(~x)*sin(~y) => (cos(x-y)-cos(x+y))/2,  
                  cos(~x)^2       => (1+cos(2*x))/2,  
                  sin(~x)^2       => (1-cos(2*x))/2};
```
Such named rule lists may be inspected as needed. E.g., the command `R
trig1"` would cause the above list to be printed.

Rule lists may be used in two ways. They can be globally instantiated by means of the command `let`. For example,
```Julia
julia> Algebra.rlet(:trig1)
```
would cause the above list of rules to be globally active from then on until cancelled by the command `clearrules`, as in
```Julia
julia> Algebra.clearrules(:trig1)
```

```@docs
Reduce.Algebra.clearrules
```

The second way to use rule lists is to invoke them locally by means of a `where` clause. For example
```
        cos(a)*cos(b+c)  
           where {cos(~x)*cos(~y) => (cos(x+y)+cos(x-y))/2};
```
or
```
R"cos(a)*sin(b) where trigrules"
```
The syntax of an expression with a `where` clause is:
```
        <expression>  
            WHERE <rule>|<rule list>(,<rule>|<rule list> ...)
```
so the first example above could also be written
```
        cos(a)*cos(b+c)  
           where cos(~x)*cos(~y) => (cos(x+y)+cos(x-y))/2;
```
The effect of this construct is that the rule list(s) in the `where` clause only apply to the expression on the left of `where`. They have no effect outside the expression. In particular, they do not affect previously defined `where` clauses or `let` statements. For example, the sequence
```
     let a=2;  
     a where a=>4;  
     a;
```
would result in the output
```
     4  
 
     2
```
Although `where` has a precedence less than any other infix operator, it still binds higher than keywords such as `else`, `then`, `do`, `repeat` and so on. Thus the expression
```Julia
R"if a=2 then 3 else a+2 where a=3"
```
will parse as
```Julia
R"if a=2 then 3 else (a+2 where a=3)"
```
`where` may be used to introduce auxiliary variables in symbolic mode expressions, as described in Section 17.4. However, the symbolic mode use has different semantics, so expressions do not carry from one mode to the other.

*Compatibility Note:* In order to provide compatibility with older versions of rule lists released through the Network Library, it is currently possible to use an equal sign interchangeably with the replacement sign `=>` in rules and `let` statements. However, since this will change in future versions, the replacement sign is preferable in rules and the equal sign in non-rule-based `let` statements.

#### Advanced Use of Rule Lists

Some advanced features of the rule list mechanism make it possible to write more complicated rules than those discussed so far, and in many cases to write more compact rule lists. These features are:

* Free operators
* Double slash operator
* Double tilde variables.

A *free operator* in the left hand side of a pattern will match any operator with the same number of arguments. The free operator is written in the same style as a variable. For example, the implementation of the product rule of differentiation can be written as:
```
operator diff, !~f, !~g;  
 
prule := {diff(~f(~x) * ~g(~x),x) =>  
             diff(f(x),x) * g(x) + diff(g(x),x) * f(x)};  
 
let prule;  
 
diff(sin(z)*cos(z),z);  
 
         cos(z)*diff(sin(z),z) + diff(cos(z),z)*sin(z)
```
The *double slash operator* may be used as an alternative to a single slash (quotient) in order to match quotients properly. E.g., in the example of the Gamma function above, one can use:
```
gammarule :=  
   {gamma(~z)//(~c*gamma(~zz))  => gamma(z)/(c*gamma(zz-1)*zz)  
                  when fixp(zz -z) and (zz -z) >0,  
    gamma(~z)//gamma(~zz) => gamma(z)/(gamma(zz-1)*zz)  
                  when fixp(zz -z) and (zz -z) >0};  
 
let gammarule;  
 
gamma(z)/gamma(z+3);  
 
          1  
----------------------  
  3      2  
 z  + 6*z  + 11*z + 6
```
The above example suffers from the fact that two rules had to be written in order to perform the required operation. This can be simplified by the use of *double tilde variables*. E.g. the rule list
```
 GGrule :=  {  
    gamma(~z)//(~~c*gamma(~zz))  => gamma(z)/(c*gamma(zz-1)*zz)  
     when fixp(zz -z) and (zz -z) >0};
```
will implement the same operation in a much more compact way. In general, double tilde variables are bound to the neutral element with respect to the operation in which they are used.
```
Pattern given	Argument used	Binding
~z + ~~y	x	z=x; y=0
~z + ~~y	x+3	z=x; y=3 or z=3; y=x
~z * ~~y	x	z=x; y=1
~z * ~~y	x*3	z=x; y=3 or z=3; y=x
~z / ~~y	x	z=x; y=1
~z / ~~y	x/3	z=x; y=3
```
Remarks: A double tilde variable as the numerator of a pattern is not allowed. Also, using double tilde variables may lead to recursion errors when the zero case is not handled properly.
```
let f(~~a * ~x,x)  => a * f(x,x) when freeof (a,x);  
 
f(z,z);  
 
***** f(z,z) improperly defined in terms of itself  
 
% BUT:  
 
let ff(~~a * ~x,x)  
       => a * ff(x,x) when freeof (a,x) and a neq 1;  
 
ff(z,z);  
                 ff(z,z)  
 
ff(3*z,z);  
                 3*ff(z,z)
```

#### Displaying Rules Associated with an Operator

```@docs
Reduce.Algebra.showrules
```

#### Order of Application of Rules

If rules have overlapping domains, their order of application is important. In general, it is very difficult to specify this order precisely, so that it is best to assume that the order is arbitrary. However, if only one operator is involved, the order of application of the rules for this operator can be determined from the following:

1. Rules containing at least one free variable apply before all rules without free variables.
2. Rules activated in the most recent `let` command are applied first.
3. `let` with several entries generate the same order of application as a corresponding sequence of commands with one rule or rule set each.
4. Within a rule set, the rules containing at least one free variable are applied in their given order. In other words, the first member of the list is applied first.
5. Consistent with the first item, any rule in a rule list that contains no free variables is applied after all rules containing free variables.

*Example:* The following rule set enables the computation of exact values of the Gamma function:
```
        operator gamma,gamma_error;  
        gamma_rules :=  
        {gamma(~x)=>sqrt(pi)/2 when x=1/2,  
         gamma(~n)=>factorial(n-1) when fixp n and n>0,  
         gamma(~n)=>gamma_error(n) when fixp n,  
         gamma(~x)=>(x-1)*gamma(x-1) when fixp(2*x) and x>1,  
         gamma(~x)=>gamma(x+1)/x when fixp(2*x)};
```
Here, rule by rule, cases of known or definitely uncomputable values are sorted out; e.g. the rule leading to the error expression will be applied for negative integers only, since the positive integers are caught by the preceding rule, and the last rule will apply for negative odd multiples of 1∕2 only. Alternatively the first rule could have been written as
```
        gamma(1/2) => sqrt(pi)/2
```
but then the case ``x = 1∕2`` should be excluded in the `when` part of the last rule explicitly because a rule without free variables cannot take precedence over the other rules.

## 11.4 Asymptotic Commands

In expansions of polynomials involving variables that are known to be small, it is often desirable to throw away all powers of these variables beyond a certain point to avoid unnecessary computation. The command `let` may be used to do this. For example, if only powers of `x` up to `x^7` are needed, the command
```Julia
julia> Algebra.rlet(:(x^8) => 0)
```
will cause the system to delete all powers of `x` higher than 7.

*CAUTION:* This particular simplification works differently from most substitution mechanisms in REDUCE in that it is applied during polynomial manipulation rather than to the whole evaluated expression. Thus, with the above rule in effect, `x^10/x^5` would give the result zero, since the numerator would simplify to zero. Similarly `x^20/x^10` would give a `Zero divisor` error message, since both numerator and denominator would first simplify to zero.

The method just described is not adequate when expressions involve several variables having different degrees of smallness. In this case, it is necessary to supply an asymptotic weight to each variable and count up the total weight of each product in an expanded expression before deciding whether to keep the term or not. There are two associated commands in the system to permit this type of asymptotic constraint. The command WEIGHT takes a list of equations of the form
```
⟨kernel form⟩ = ⟨number⟩
```
where `⟨number⟩` must be a positive integer (not just evaluate to a positive integer). This command assigns the weight `⟨number⟩` to the relevant kernel form. A check is then made in all algebraic evaluations to see if the total weight of the term is greater than the weight level assigned to the calculation. If it is, the term is deleted. To compute the total weight of a product, the individual weights of each kernel form are multiplied by their corresponding powers and then added.

The weight level of the system is initially set to 1. The user may change this setting by the command
```Julia
R"wtlevel <number>"
```
which sets `⟨number⟩` as the new weight level of the system. `meta` must evaluate to a positive integer. `wtlevel` will also allow `nil` as an argument, in which case the current weight level is returned.
