# Appendix C: Changes since Version 3.8

**New packages** assert bibasis breduce cde cdiff clprl gcref guardian lalr lessons libreduce listvecops lpdo redfront reduce4 sstools utf8

**Core package rlisp** Support for namespaces (::)

Default value in switch statement

Support for utf8 characters

**Core package poly** Improvements for differentiation: new switches **expanddf**, **allowdfint** etc (from odesolve)

**Core package alg** New switch **precise_complex**

Improvements for switch **combineexpt** (exptchk.red)

New command **unset**

New operators **continued_fraction**, **totaldeg**

Operators now defined in the REDUCE core:

> changevar, si, ci, gamma, igamma, psi, polygamma, beta, ibeta, euler, bernoulli, pochhammer, lerch_phi, polylog, zeta, besselj, bessely, besseli, besselk, hankel1, hankel2, kummerM, kummerU, struveh, struvel, lommel1, lommel2, whittakerm, whittakerw, Airy_Ai, Airy_Bi, Airy_AiPrime, Airy_biprime, binomial, solidharmonic, sphericalharmonic, fibonacci,fibonaccip, motzkin, hypergeometric, MeijerG.

Constants now part of the core:

now known as part of the core, as well as constants **catalan**, **euler_gamma**, **golden_ratio**, **khinchin**.

**Core Package solve** New boolean operator **polyp**(p,var), to determine whether p is a pure polynomial in var, ie. the coefficients of p do not contain var.

**Core Package matrix** New keyword **matrixproc** for declaration of matrix-valued procedures.

**Package defint** Added **trdefint** switch for tracing.
