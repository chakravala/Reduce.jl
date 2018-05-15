# Appendix A: Reserved Identifiers

We list here all identifiers that are normally reserved in REDUCE including names of commands, operators and switches initially in the system. Excluded are words that are reserved in specific implementations of the system.

Commands
> algebraic antisymmetric array bye clear clearrules comment cont decompose define depend display ed editdef end even factor for forall foreach go goto if in index infix input integer korder let linear lisp listargp load load_package mass match matrix matrixproc mshell nodepend noncom nonzero nospur odd off on operator order out pause precedence print_precision procedure quit real remember remfac remind retry return saveas scalar setmod share showtime shut spur symbolic symmetric unset vecdim vector weight write wtlevel

Boolean Operators
> evenp fixp freeof numberp ordp primep

Infix Operators
> := = >= > <= < => + - \* / // ^ \*\* . .. where setq or and member memq equal neq eq geq greaterp leq lessp plus difference minus times quotient recip expt cons

Numerical Operators
> abs acos acosh acot acoth acsc acsch airy_ai airy_aiprime airy_bi airy_biprime asec asech asin asinh atan atanh atan2 bernoulli besseli besselj besselk bessely beta cos cosh cot coth csc csch csch exp factorial fix floor gamma hankel1 hankel2 hypot ibeta igamma kummerm kummeru lerch_phi ln log logb log10 lommel1 lommel2 nextprime pochhammer polygamma psi round sec sech sin sinh sqrt struveh struvel tan tanh whittakerm whittakeru zeta

Prefix Operators
> append arbcomplex arbint arglength ceiling ci coeff coeffn cofactor conj continued_fraction deg den det df dilog ei eps erf expand_cases factorize fibonacci fibonaccip first gcd g hypergeometric impart int interpol lcm lcof length lhs linelength list lpower lterm mainvar map mat mateigen max meijerg min mkid motzkin nullspace num one_of part pf precision prod random random_new_seed rank rederr reduct remainder repart rest resultant reverse rhs root_of root_val second select set showrules si sign solve solidharmonicy sphericalharmonicy structr sub sum third totaldeg tp trace varname

Reserved Variables
> \_line_ assumptions card_no catalan e euler_gamma eval_mode fort_width golden_ratio high_pow i infinity k!\* khinchin low_pow negative nil pi positive requirements root_multiplicities t

Switches
> adjprec algint allbranch allfac allowdfint arbvars balance_mod bezout bfspace combineexpt combinelogs commutedf comp complex cramer cref defn demo dfint div echo errcont evallhseqp exp expanddf expandlogs ezgcd factor failhard fort fortupper fullroots gcd ifactor int intstr lcm list listargs mcd modular msg multiplicities nat nero nocommutedf noconvert nolnr nosplit output period precise precise_complex pret pri rat ratarg rational rationalize ratpri revpri rlisp88 roundall roundbf rounded savestructr simpnoncomdf solvesingular time tra trdefint trfac trigform trint varopt

OtherReservedIds
> begin do then expr fexpr input lambda lisp macro product repeat smacro sum then until when while ws
