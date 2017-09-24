#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

sbas = [
    :abs,
    :conj,
    :factorial,
    :floor,
    :max,
    :min,
    :round,
    :sign,
    :acos,
    :acosh,
    :acot,
    :acoth,
    :acsc,
    :acsch,
    :asec,
    :asech,
    :asin,
    :asinh,
    :atan,
    :atanh,
    :atan2,
    :cos,
    :cosh,
    :cot,
    :coth,
    :csc,
    :csch,
    :exp,
    :hypot,
    :log,
    :log10,
    :sec,
    :sech,
    :sin,
    :sinh,
    :sqrt,
    :tan,
    :tanh,
    :gamma
]

sdep = [
    :beta,
    :besseli,
    :besselj,
    :besselk,
    :bessely,
    :polygamma,
    :zeta
]

sfun = [
#    :logb,
    :ibeta,
    :igamma,
    :ln,
    :psi,
    :bernoulli,
    :continued_fraction,
    :ci, #
    :dilog,
    :ei,
    :si,
    :airy_ai,
    :airy_aiprime,
    :airy_bi,
    :airy_biprime,
    :hanekl1,
    :hankel2,
    :kummerm,
    :kummeru,
    :lommel1,
    :lommel2,
    :struveh,
    :struvel,
    :whittakerm,
    :whittakeru,
    :solidharmonicy,
    :sphericalharmonicy
]

snum = [
    :ceiling,
    :fix,
]

scom = [
    :impart,
    :repart,
]

sint = [
    :nextprime,
    :euler,
    :fibonacci,
    :motzkin,
]

sran = [
    :random,
    :random_new_seed
]

Expr(:toplevel,[:(import Base: $i) for i ∈ [sbas;sdep;[:length]]]...) |> eval
:(export $([sbas;sdep;sfun;snum;scom;sint;sran;[:length]]...)) |> eval

for fun in [sbas;sdep;sfun;snum;scom;sint;sran;[:length]]
    rfun = Symbol(:r,fun)
    quote
        function $fun(r::RExpr,be=0)
            nsr = Compat.String[]
            sexpr = split(r).str
            iter = 1:length(sexpr)
            state = start(iter); #show(sexpr)
            while !done(iter,state)
                (h,state) = next(iter, state)
                sh = split(sexpr[h],r"[ ]+")
                en = 1
                isempty(replace(sh[en]," ","")) && (en = 2); #show(sh[en])
                if contains(sh[en], "procedure")
                    js = join(split(sexpr[h],"procedure")[2:end],"procedure")
                    (h,state) = next(iter, state)
                    y = h
                    (h,state) = bematch(sexpr[h],sexpr,h,iter,state)
                    push!(nsr,Compat.String("procedure "*js))
                    push!(nsr,$rfun(sexpr[y:h],be) |> string)
                elseif contains(sh[en], "begin")
                    js = join(split(sexpr[h],"begin")[2:end],"begin")
                    ep = Array{Compat.String,1}(length(0))
                    sh1 = split(js,r"[ ]+")
                    c = sum(sh1.=="begin")-sum(sh1.=="end")
                    flag = c ≥ 0
                    c ≤ -1 && (js = join(split(js,"end")[1:end+c],"end"))
                    y = h
                    (h,state) = bematch(js,sexpr,h,iter,state)
                    ep[1] = $rfun(vcat(js,sexpr[y+1:h]...),be+1) |> string
                    ep[1] == nothing && shift!(ep)
                    while !done(iter,state) & flag
                        (h,state) = next(iter, state)
                        cQ = c
                        js = sexpr[h]
                        sh2 = split(js,r"[ ]+")
                        c += sum(sh2.=="begin")-sum(sh2.=="end")
                        c ≤ -1 && (js = join(split(js,"end")[1:end+c],"end"))
                        y = h
                        (h,state) = bematch(js,sexpr,h,iter,state)
                        epr = $rfun(vcat(js,sexpr[y+1:h]...),cQ) |> string
                        epr ≠ nothing && push!(ep,epr)
                    end
                    push!(nsr,ep...)
                elseif contains(sh[en],"return")
                    js = join(split(sexpr[h],"return")[2:end],"return")
                    y = h
                    (h,state) = bematch(js,sexpr,h,iter,state)
                    rp = $rfun(vcat(js,sexpr[y+1:h]...),be) |> string
                    push!(nsr,"return "*rp)
                elseif contains(sh[en],"end")
                    nothing
                elseif isempty(sh[en])
                    nothing
                elseif contains(sexpr[h], ":=")
                    sp = split(sexpr[h], ":=")
                    push!(nsr, Compat.String(sp[1]) * ":=" * string($rfun(sp[2] |> Compat.String |> RExpr,be)))
                elseif contains(sexpr[h],":")
                    sp = split(sexpr[h],":")
                    push!(nsr,($rfun(sp[1],be) |> string) * ":" * ($rfun(sp[2],be) |> string))
                else
                    js=sexpr[h]
                    se=sum(sh.=="end")
                    0<se≤be ? (js=replace(js,"end","")) :
                        (se>be && (js=join(split(js,"end")[1:end-be],"end")))
                    push!(nsr, $(string(fun)) * "($js)" |> RExpr |> rcall)
                end
            end
            return RExpr(nsr)
        end
        $rfun(r::Array{Compat.String,1},be=0) = $fun(RExpr(r),be)
        $rfun(r,be=0) = $fun(r |> Compat.String |> RExpr, be)
    end |> eval
end

for fun in [sbas;sdep;sfun;snum;scom;sint]
    for T in [:(Compat.String),:Expr]
        quote
            function $fun(expr::$T,be=0)
                convert($T, $fun(RExpr(expr),be))
            end
        end |> eval
    end
end

length(r::Expr,be=0) = r |> RExpr |> length |> parse |> eval

for fun in [sint;sran]
    quote
        function $fun{T<:Integer}(n::T)
            convert(T, $fun(RExpr(n)) |> parse |> eval)
        end
    end |> eval
end

for fun in snum
    quote
        function $fun{T<:Real}(x::T)
            $fun(RExpr(x)) |> parse |> eval
        end
    end |> eval
end

for fun in scom
    quote
        function $fun{T<:Number}(x::T)
            $fun(RExpr(x)) |> parse |> eval
        end
    end |> eval
end

function bernoulli{T<:Integer}(n::T)
    bernoulli(RExpr(n)) |> parse |> eval
end
