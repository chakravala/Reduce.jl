#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

const switchbas = [
    :complex,
    :div,
    :lcm,
    :gcd,
]

const switches = [
    :factor,
    :ifactor,
    :expandlog,
    :combinelog,
    :precise,
    :combineexpt,
    :rounded,
    :evallhseq,
    :horner,
    :trigform,
    :fullroots,
    :cramer,
    :multiplicities,
    :allbranch,
    :arbvars,
    :solvesingular,
    :varopt,
    :intstr,
    :allfac,
    :rat,
    :revpri,
    :fort,
    :ratarg,
    :ezgcd,
    :heugcd,
    :bezout,
    :modular,
    :rational,
    :roundbf,
    :adjprec,
    :roundall,
    :balancedmod,
    :nocommutedf,
    :commutedf,
    :simpnoncomdf,
    :expanddf,
    :allowdfint,
    :dfint,
    :trint,
    :trintsubst,
    :failhard,
    :nolnr,
    :nointsubst,
]

push!(VERSION < v"1.0-" ? switchbas : switches, :expand)

const switchtex = [
    :nat,
    :latex
]

const onswitch = Symbol.("@",[switchbas;switches])
const offswitch = Symbol.("@off_",[switchbas;switches])

Expr(:toplevel,[:(import Base: $i) for i ∈ switchbas]...) |> eval
:(export $([switchbas;switches;switchtex]...)) |> eval
:(export $([onswitch;offswitch;Symbol.("@",switchtex)]...)) |> eval

for fun in [switchbas;switches;switchtex]
    @eval begin
        $(parsegen(fun,:switch))
        $fun(s::Bool) = (rcall("$(s ? "on" : "off") "*$(string(fun))); s)
    end
end

for fun in [switchbas;switches]
    @eval begin
        $(unfoldgen(fun,:switch))
        function $fun(expr::String;be=0)
            convert(String, $fun(RExpr(expr);be=be))
        end
    end
end

for fun in switchtex
    @eval begin
        $(unfoldgen(fun,:switch))
        function $fun(expr::String;be=0)
            convert(String, $fun(RExpr(expr);be=be))
        end
        macro $fun(expr)
            $fun(expr)
        end
    end
end

for fun in [switchbas;switches]
    @eval begin
        macro $fun(expr)
            (r,on,off) = macroshift(expr)
            push!(on,$(string(fun)))
            Expr(:quote,rcall(r;on=on,off=off))
        end
        macro $(Symbol("off_",fun))(expr)
            (r,on,off) = macroshift(expr)
            push!(off,$(string(fun)))
            Expr(:quote,rcall(r;on=on,off=off))
        end
    end
end

function macroshift(r)
    if typeof(r) == Expr && r.head == :macrocall
        (expr,on,off) = macroshift(r.args[3])
        if r.args[1] ∈ onswitch
            (expr,push!(on,string(r.args[1])[2:end]),off)
        elseif r.args[1] ∈ offswitch
            (expr,on,push!(off,string(r.args[1])[6:end]))
        else
            (eval(r),String[],String[])
        end
    else
        ((typeof(r)==Expr && r.head==:quote) ? r.args[1] : r,String[],String[])
    end
end

@doc """
    multiplicities(::Bool)

If you want the multiplicities explicitly displayed, the switch `multiplicities` can be turned on. For example
```Julia
julia> Algebra.on(:multiplicities); Algebra.solve(:(x^2==2x-1),:x)
```
yields the result
```Julia
(:(x = 1), :(x = 1))
```
""" Reduce.multiplicities
