#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

export unfold, mat, treecombine!, irr

"""
REDUCE begin and end marker counter for parsegen
"""
@inline function becount(js,openpar,closepar)
    if typeof(openpar) == String
        sh = split(js,r"[ ]+")
        sum(sh.==openpar)-sum(sh.==closepar)
    else
        count(z->z==openpar,js)-count(z->z==closepar,js)
    end
end

"""
REDUCE begin and end marker match for parsegen
"""
@noinline function bematch(js,sexpr,iter,next,openpar,closepar)
    (h,state) = next
    y = h
    c = becount(js,openpar,closepar)
    flag = c > 0
    nxt = iterate(iter, state)
    while (nxt !== nothing) & flag
        (y,state) = nxt
        c += becount(sexpr[y],openpar,closepar)
        flag = c > 0
        flag && (nxt = iterate(iter, state))
    end
    return (y,state)
end

const prefix = r"(?<!\))(([A-Za-z_][A-Za-z_0-9]*)|([\^+\/-])|([*]{1,2})|(- ))(?=\()"
const parens = r"\(((?>[^\(\)]+)|(?R))*\)"
const braces = r"{((?>[^{}]+)|(?R))*}"
const infix1 = r"^(([\^\+\/])|([*]{1,2})|( -)|( \+)|( [*]{1,2})|( /)|( \^))"
const infix2 = r"(([\^+\/])|([*]{1,2}))$"
const assign = r"^([A-Za-z_ ][A-Za-z_0-9 ]*)(:=)"

@inline function argrfun(mode::Symbol,rfun::Symbol,sep,be=:be)
    if mode == :expr
        :($rfun(fun,$sep;be=$be))
    elseif mode == :args
        :($rfun(fun,$sep,s;be=$be) |> string)
    else
        :($rfun(fun,$sep;be=$be) |> string)
    end
end

function loopshift(js,openpar,closepar,T,sexpr,iter,next)
    ep = Array{T,1}(undef,0)
    c = becount(js,openpar,closepar)
    flag = c ≥ 0
    !flag && (js = join(split(js,closepar)[1:end+c],closepar))
    (h,state) = next
    y = h
    nxt = bematch(js,sexpr,iter,next,openpar,closepar)
    (h,state) = nxt
    push!(ep,js,sexpr[y+1:h]...)
    ep[1] == nothing && popfirst!(ep)
    nxt = iterate(iter, state)
    while (nxt !== nothing) & flag
        (h,state) = nxt
        cQ = c
        js = sexpr[h]
        c += becount(js,openpar,closepar)
        if c ≤ -1
            js = join(split(js,closepar)[1:end+c],closepar)
            flag = false
        end
        y = h
        nxt = bematch(js,sexpr,iter,nxt,openpar,closepar)
        (h,state) = nxt
        epr = vcat(js,sexpr[y+1:h]...)
        epr ≠ nothing && push!(ep,epr...)
        flag && (nxt = iterate(iter, state))
    end
    return ((h,state),ep)
end

for mode ∈ [:expr,:unary,:switch,:args]
    rfun = Symbol(:r,"_",mode)
    modefun = Symbol(:parse,"_",mode)
    argfun = Symbol(:args,"_",mode)
    arty = (mode == :expr) ? :Any : :String
    exec = if mode == :expr
        :(Meta.parse(RSymReplace(js)))
    elseif mode == :unary
        :("$fun($js)" |> RExpr |> rcall)
    elseif mode == :switch
        :(rcall("$js" |> RExpr, fun))
    elseif mode == :args
        :("$fun($(join([js,s...],',')))" |> RExpr |> rcall)
    end
    mode != :args ? (fargs = [:(r::RExpr)]) : (fargs = [:(r::RExpr),Expr(:...,:sr)])
    mode != :args ? (aargs = [:be]) : (aargs = [:be,Expr(:...,:s)])
    quote
        export $modefun
        @noinline function $modefun(fun::String,$(fargs...);be=0)
            nsr = $arty[]
            $(if mode == :args
                quote
                    s = Array{String,1}()
                    for rs ∈ sr
                        push!(s,rs |> RExpr |> string)
                    end
                end
            end)
            sexpr = split(r).str
            iter = 1:length(sexpr)
            next = iterate(iter); #show(sexpr)
            while next !== nothing
                (h,state) = next
                sh = split(sexpr[h],r"[ ]+")
                en = 1
                isempty(replace(sh[en]," "=>"")) && (en = 2); #show(sh[en])
                if occursin(r"^procedure",sh[en])
                    js = join(split(sexpr[h],"procedure")[2:end],"procedure")
                    next = iterate(iter, state)
                    (h,state) = next
                    y = h
                    next = bematch(sexpr[h],sexpr,iter,next,"begin","end")
                    (h,state) = next
                    $(mode != :expr ? :(push!(nsr,String("procedure "*js))) : :(nothing))
                    push!(nsr,$(if mode == :expr
                        :(Expr(:function,Meta.parse(js),$rfun(fun,sexpr[y:h];be=be)))
                    elseif mode == :args
                        :($rfun(fun,sexpr[y:h],s;be=be) |> string)
                    else
                        :($rfun(fun,sexpr[y:h];be=be) |> string)
                    end))
                elseif occursin(r"^begin",sh[en])
                    js = join(split(sexpr[h],"begin")[2:end],"begin")
                    ep = Array{$arty,1}(undef,0)
                    c = becount(js,"begin","end")
                    flag = c ≥ 0
                    !flag && (js = join(split(js,"end")[1:end+c],"end"))
                    y = h
                    next = bematch(js,sexpr,iter,next,"begin","end")
                    (h,state) = next
                    $(mode != :expr ? :(push!(nsr,String("begin "*js))) : :(nothing))
                    push!(ep,$(argrfun(mode,rfun,:(vcat(js,sexpr[y+1:h]...)),:(be+1))))
                    ep[1] == nothing && popfirst!(ep)
                    next = iterate(iter, state)
                    while (next !== nothing) & flag
                        (h,state) = next
                        cQ = c
                        js = sexpr[h]
                        c += becount(js,"begin","end")
                        if c ≤ -1
                            js = join(split(js,"end")[1:end+c],"end")
                            flag = false
                        end
                        y = h
                        next = bematch(js,sexpr,iter,next,"begin","end")
                        (h,state) = next
                        epr = $(argrfun(mode,rfun,:(vcat(js,sexpr[y+1:h]...)),:cQ))
                        epr ≠ nothing && push!(ep,epr)
                        flag && (next = iterate(iter, state))
                    end
                    next = (h,state)
                    push!(nsr,$((mode == :expr) ? :(Expr(:block,ep...)) : Expr(:...,:ep)))
                    $(mode != :expr ? :(push!(nsr,String("end"))) : :(nothing))
                elseif occursin(r"^return",sh[en])
                    js = join(split(sexpr[h],"return")[2:end],"return")
                    y = h
                    next = bematch(js,sexpr,iter,next,"begin","end")
                    (h,state) = next
                    rp = $(argrfun(mode,rfun,:(vcat(js,sexpr[y+1:h]...))))
                    $(mode != :expr ? :(push!(nsr,"return "*rp)) : :(push!(nsr,Expr(:return,rp))))
                elseif occursin(assign,sexpr[h])
                    sp = split(sexpr[h], ":=",limit=2)
                    push!(nsr,$(if mode == :expr
                        :(Expr(:(=),Meta.parse(sp[1]),$rfun(fun,sp[2];be=be)))
                    elseif mode == :args
                        :(String(sp[1]) * ":=" * string($rfun(fun,sp[2] |> String |> RExpr,s;be=be)))
                    else
                        :(String(sp[1]) * ":=" * string($rfun(fun,sp[2] |> String |> RExpr;be=be)))
                    end))
                elseif occursin("for",sh[en])
                    throw(ReduceError("for block parsing not yet supported"))
                elseif occursin(braces,$((mode == :expr) ? :(sexpr[h]) : :("")))
                    $(if mode == :expr; quote
                        ts = sexpr[h]
                        (next,mp) = loopshift(ts,'{','}',String,sexpr,iter,next)
                        (h,state) = next
                        smp = match(braces,join(mp,";\n")).match[2:end-1]
                        ListPrint(ListPrint()+1)
                        args = Array{$arty,1}(undef,0)
                        while smp ≠ ""
                            lsM = match(braces,smp)
                            lsm = lsM ≠ nothing ? lsM.match[2:end-1] : smp
                            lsd = split(smp,braces;limit=2)
                            pre = length(lsd)≠1 ? lsd[1][1:end-1] : ""
                            lsM == nothing && (pre = join([pre,lsm],','))
                            if pre ≠ ""
                                af = $argfun(fun,$(string(mode)),split(pre,','),be)
                                for k ∈ af
                                    k≠nothing && push!(args, k≠[nothing] ? k : Array{Any,1}(undef,0))
                                end
                            end
                            smp = length(lsd)≠1 ? lsd[end][2:end] : ""
                            if lsM ≠ nothing
                                af = $(argrfun(mode,rfun,:(lsM.match)))
                                push!(args, af≠[nothing] ? af : Array{Any,1}(undef,0))
                            end
                        end
                        ListPrint(ListPrint()-1)
                        length(args)==1 && typeof(args[1]) <: Tuple && (args = args[1])
                        push!(nsr,Tuple(args))
                    end; else; :(nothing); end)
                elseif occursin("=>",$((mode == :expr) ? :(sexpr[h]) : :("")))
                    sp = split(sexpr[h],r"=>")
                    stuff = String.(split(sp[2],r"(when)|(and)"))
                    ar = $rfun.(fun,stuff;be=be)
                    push!(nsr,$rfun(fun,sp[1];be=be) => length(ar) ≠ 1 ? ar : ar[1])
                elseif occursin(prefix,$((mode == :expr) ? :(sexpr[h]) : :("")))
                    $(if mode == :expr; quote
                    ts = sexpr[h]
                    (next,mp) = loopshift(ts,'(',')',String,sexpr,iter,next)
                    (h,state) = next
                    smp = replace(join(mp,";\n"),"**"=>'^')
                    qr = IOBuffer()
                    while smp ≠ ""
                        args = Array{$arty,1}(undef,0)
                        if occursin(r"^\s?\(",smp)
                            push!(args,$(argrfun(mode,rfun,:(match(parens,smp).match[2:end-1]))))
                            smp = split(smp,parens;limit=2)[end]
                            print(qr,"($(args...))")
                            if !occursin(prefix,smp)
                                $(if mode == :expr; quote
                                    if occursin(infix1,smp)
                                        print(qr, RSymReplace(match(infix1,smp).match), RSymReplace(split(smp,infix1)[end]))
                                        smp = ""
                                    else
                                        print(qr, RSymReplace(smp))
                                        smp = ""
                                    end; end
                                else
                                    :(print(qr, smp); smp = "")
                                end)
                            elseif occursin(infix1,smp)
                                print(qr, RSymReplace(match(infix1,smp).match))
                                smp = split(smp,infix1)[end]
                            end
                            continue
                        end
                        pf = match(prefix,smp).match |> String
                        sp = split(smp,prefix;limit=2)
                        $(if mode == :expr; quote 
                            if occursin(infix2,sp[1])
                                rq = split(sp[1],infix2)[1]
                                if occursin(infix1,rq)
                                    rq = RSymReplace(match(infix1,rq).match) * RSymReplace(split(rq,infix1)[end])
                                else
                                    rq = RSymReplace(rq)
                                end
                                print(qr, rq, RSymReplace(match(infix2,sp[1]).match))
                            elseif occursin(infix1,sp[1])
                                print(qr, RSymReplace(match(infix1,sp[1]).match), RSymReplace(split(sp[1],infix1)[end]))
                            else
                                print(qr, RSymReplace(sp[1]))
                            end; end
                        else
                            :(print(qr, sp[1]))
                        end)
                        smp = split(sp[2],parens;limit=2)[end]
                        lsm = match(parens,sp[2]).match[2:end-1]
                        if pf == "mat"
                            mt = collect((m.match for m=eachmatch(parens,lsm)))
                            for row ∈ mt
                                elm = split(row[2:end-1],',')
                                push!(args,$(if mode == :expr
                                    :(Expr(:row,$argfun(fun,$(string(mode)),elm,be)...))
                                elseif mode == :args
                                    :($argfun(fun,$(string(mode)),elm,be,s) |> string)
                                else
                                    :($argfun(fun,$(string(mode)),elm,be) |> string)
                                end))
                            end
                            print(qr, $(if mode == :expr
                                :(Expr(:vcat,args...) |> string)
                            else
                                :("$pf($(join(args,',')))")
                            end))
                        else
                            ls = split(lsm,',')
                            push!(args,$(if mode == :expr
                                :($argfun(fun,$(string(mode)),ls,be))
                            elseif mode == :args
                                :($argfun(fun,$(string(mode)),ls,be,s) |> string)
                            else
                                :($argfun(fun,$(string(mode)),ls,be) |> string)
                            end)...)
                            rq = "$(RSymReplace(pf))($(join(args,',')))"
                            print(qr, $(if mode == :expr
                                :(((isinfix(pf) && length(args) == 1) ? rq : "($rq)"))
                            else
                                :("$pf($(join(args,',')))")
                            end))
                        end
                        !occursin(prefix,smp) && ($(if mode == :expr; quote
                            if occursin(infix1,smp)
                                print(qr, RSymReplace(match(infix1,smp).match), RSymReplace(split(smp,infix1)[end]))
                                smp = ""
                            else
                                print(qr, RSymReplace(smp))
                                smp = ""
                            end; end
                        else
                            :(print(qr, smp); smp = "")
                        end))
                    end
                    push!(nsr,$((mode == :expr) ? :("("*String(take!(qr))*")" |> Meta.parse |> linefilter!) : :qr))
                    end; else; :(nothing); end)
                elseif occursin("end",sh[en])
                    nothing
                elseif isempty(sh[en])
                    nothing
                elseif occursin("=",$((mode == :expr) ? :(sexpr[h]) : :("")))
                    $(if mode == :expr; quote
                    sp = split(sexpr[h],"=")
                    push!(nsr,$(:(Expr((ListPrint()>0 ? (:(=),) : (:call,:(==)))...,
                            $rfun(fun,sp[1];be=be),$rfun(fun,sp[2];be=be)))))
                    end; end)
                elseif occursin(":",sexpr[h])
                    sp = split(sexpr[h],":")
                    push!(nsr,$(if mode == :expr
                        :(Expr(:(:),$rfun(fun,sp[1];be=be),$rfun(fun,sp[2];be=be)))
                    elseif mode == :args
                        :(($rfun(fun,sp[1],[];be=be)|>string)*":"*($rfun(fun,sp[2],s;be=be)|>string))
                    else
                        :(($rfun(fun,sp[1];be=be)|>string)*":"*($rfun(fun,sp[2];be=be)|>string))
                    end))
                else
                    js=sexpr[h]
                    se=sum(sh.=="end")
                    0<se≤be ? (js=replace(js,"end","")) :
                        (se>be && (js=join(split(js,"end")[1:end-be],"end")))
                    exc = $exec
                    $(if mode ≠ :expr
                        :(if string(exc) == ""
                            c = 1
                            f = SubFail()
                            H = SubHold()
                            while string(exc) == "" && c < f
                                sleep(sqrt(c)*H)
                                exc = $exec
                                c += 1
                            end
                            PipeClogged(string(exc) ≠ "", c, "$fun function")
                            string(exc) == "" && throw(ReduceError(if fun == "//"
                                "If generated code has many calls to $fun, try setting `Reduce.Rational(false)` in code, since rational division is the default; or try `Reduce.Reset()`."
                            else
                                "If generated code has many calls to $fun, try to minimize the number of calls with REDUCE switches and use `Reduce.Reset()` if you'd like to start a new pipe."
                            end))
                        end)
                    else; nothing
                    end)
                    push!(nsr, exc)
                end
                next = iterate(iter, state)
            end
            $(if mode == :expr
                quote
                    u = length(nsr)
                    return u==1 ? nsr[1] : (u==0 ? nothing : Expr(:block,nsr...))
                end
            else
                :(if fun in ["nat","latex"]
                    return nsr |> RExpr |> split |> string
                else
                    return nsr |> RExpr |> split
                end)
            end)
        end
        $(if mode == :args
            quote
                $rfun(fun::String,r::Array{String,1},s;be=0) = $modefun(fun,RExpr(r),s...;be=be)
                $rfun(fun::String,r,s;be=0) = $modefun(fun,r |> String |> RExpr,s...;be=be)
            end
        else
            quote
                $rfun(fun::String,r::Array{String,1};be=0) = $modefun(fun,RExpr(r);be=be)
                $rfun(fun::String,r;be=0) = $modefun(fun,r |> String |> RExpr;be=be)
            end
        end)
        @noinline function $argfun(fun::String,mod::String,ls::Array{SubString{String},1},$(aargs...))
            mode = Symbol(mod)
            args = Array{$arty,1}(undef,0)
            lsi = 1:length(ls)
            nxt = iterate(lsi)
            (lsh,lss) = nxt
            while nxt !== nothing
                (lsh,lss) = nxt
                if occursin(r"^begin",ls[lsh])
                    js = join(split(ls[lsh],"begin")[2:end],"begin")
                    (nxt,ep) = loopshift(js,"begin","end",$arty,ls,lsi,nxt)
                    (lsh,lss) = nxt
                    sep = "begin $(join(ep,',')) end"
                    push!(args,$(argrfun(mode,rfun,:sep)))
                elseif occursin(prefix,ls[lsh])
                    js = ls[lsh]
                    ep = Array{$arty,1}(undef,0)
                    c = count(z->z=='(',js)-count(z->z==')',js)-1
                    flag = c ≥ -1
                    !flag && (js = join(split(js,')')[1:end+c],')'))
                    lsy = lsh
                    nxt = bematch(js,ls,lsi,nxt,'(',')')
                    (lsh,lss) = nxt
                    push!(ep,js,ls[lsy+1:lsh]...)
                    ep[1] == nothing && popfirst!(ep)
                    sep = join(ep,',')
                    push!(args,$(argrfun(mode,rfun,:sep)))
                else
                    push!(args,$(argrfun(mode,rfun,:(ls[lsh]))))
                end
                nxt = iterate(lsi, lss)
            end
            return args
        end
    end |> eval
end

"""
    parsegen(::Symbol,::Symbol)

Parser generator that outputs code to walk and manipulate REDUCE expressions
"""
function parsegen(fun::Symbol,mode::Symbol)
    fune = (fun == ://) ? :/ : (fun == :rlet) ? :let : fun
    mf = Symbol(:parse,"_",mode)
    a = mode != :args ? [:(r::RExpr)] : [:(r::RExpr),Expr(:...,:s)]
    return mode ≠ :expr ? :($fun($(a...);be=0) = $mf($(string(fune)),$(a...);be=0)) :
    :($fun($(a...);be=0) = $mf($(string(fune)),$(a...);be=0) |> treecombine! |> irr)
end

"""
    treecombine!(::Expr)

Recursively simplifies out extra edges from Expr objects
"""
@noinline function treecombine!(e::Expr,redo=[false])
    for i ∈ 1:length(e.args)
        if e.args[i] |> typeof == Expr && e.args[i].head == :call
            if e.head == :call
                s = e.args[i].args[1]
                if s ∈ [://,:/]
                    if e.args[1] == :*
                        d = e.args[i].args[3]
                        e.args[i] = e.args[i].args[2]
                        e.args = [s,deepcopy(e),d]
                        redo[1] = true
                        return treecombine!(e)
                    elseif e.args[1] ∈ [://,:/]
                        if i == 2
                            e.args[3] = e.args[3]*e.args[i].args[3]
                            e.args[2] = deepcopy(e.args[i].args[2])
                        elseif i == 3
                            e.args[2] = e.args[2]*e.args[i].args[3]
                            e.args[3] = deepcopy(e.args[i].args[2])
                        end
                        redo[1] = true
                        return treecombine!(e)
                    end
                elseif s == :-
                    if e.args[1] == :-
                        if length(e.args) == 2
                            if length(e.args[i].args) == 3
                                push!(e.args,deepcopy(e.args[i].args[2]))
                                e.args[2] = deepcopy(e.args[i].args[3])
                                redo[1] = true
                                return treecombine!(e)
                            end
                        #elseif i == 2 && length(e.args) > 2
                        #    push!(e.args[i].args,e.args[3:end]...)
                        #    e.args = deepcopy(e.args[i].args)
                        #    redo[1] = true
                        #    return treecombine!(e)
                        end
                    end
                end
            elseif e.head == :(=) &&
                    e.args[2] |> typeof == Expr && e.args[2].head == :block
                    length(e.args[2].args) == 1 && (e.args[2] = e.args[2].args[1])
            end
            treecombine!(e.args[i],redo)
            d = detectinf(e)
            d ≠ nothing && (e.args[i] = d)
        else
            typeof(e.args[i]) == Expr && treecombine!(e.args[i],redo)
        end
    end
    d = detectinf(e)
    return redo[1] ? treecombine!(e) : d ≠ nothing ? d : e
end
treecombine!(e,redo=[false]) = e
treecombine!(e::T) where T <: Tuple = treecombine!.(e)

function detectinf(e)
    if typeof(e) == Expr && e.head == :call
        if e.args[1] ∈ [://,:/]
            if (e.args[2] == :NaN) | (e.args[3] == :NaN)
                return :NaN
            elseif e.args[2] == :Inf
                if typeof(e.args[3]) <: Number
                    if e.args[3] == :Inf
                        return :NaN
                    else
                        return :Inf
                    end
                elseif (typeof(e.args[3]) == Expr) && (e.args[3].head == :macrocall)
                    if e.args[3].args[1] == Symbol("@big_str") || e.args[3].args[1] == Symbol("@int128_str")
                        return :Inf
                    end
                end
            elseif e.args[3] == :Inf
                if typeof(e.args[2]) <: Number
                    return 0
                elseif (typeof(e.args[2]) == Expr) && (e.args[2].head == :macrocall)
                    if e.args[2].args[1] == Symbol("@big_str") || e.args[2].args[1] == Symbol("@int128_str")
                        return 0
                    end
                end
            end
        elseif e.args[1] ∈ [:*,:+,:-]
            found = false
            for arg ∈ e.args[2:end]
                arg == :NaN && (found = true; return :NaN)
            end
        end
    end
    return nothing
end

@inline irr(expr) = expr ∈ [:ℯ,:π,:(MathConstants.γ),:(MathConstants.φ),:Inf,:NaN] ? eval(expr) : expr

parsegen(:parse,:expr) |> eval

@doc """
    Reduce.parse(r::RExpr)

Parse a Reduce expression into a Julia expression

# Examples
```julia-repl
julia> Reduce.parse(R\"sin(i*x)\")
:(sin(im * x))
```
""" Reduce.parse

function print_args(io::IO,a::Array{Any,1})
    print(io, "(")
    for (i, arg) in enumerate(a)
        show_expr(io, arg)
        i ≠ lastindex(a) ? print(io,",") : print(io,")")
    end
end

@noinline function show_expr(io::IO, expr::Expr) # recursively unparse Julia expression
    if expr.head == :call
        if expr.args[1] == :(:)
            show_expr(io,expr.args[2])
            print(io,":")
            show_expr(io,expr.args[3])
            print(io," ")
        elseif expr.args[1] == :(==)
            show_expr(io,expr.args[2])
            print(io,"=")
            show_expr(io,expr.args[3])
        else
            show_expr(io, expr.args[1])
            print_args(io,expr.args[2:end])
        end
    elseif expr.head == :(=)
        if (typeof(expr.args[1]) == Expr) && (expr.args[1].head == :call) && ListPrint()<1
            show_expr(io,Expr(:function,expr.args[1],expr.args[2]))
        else
            show_expr(io,expr.args[1])
            print(io,ListPrint()>0 ? "=" : ":=")
            show_expr(io,expr.args[2])
        end
    elseif occursin(r"[*\/+-^]=$",string(expr.head))
        if (typeof(expr.args[1]) == Expr) && (expr.args[1].head == :call)
            throw(ReduceError("function assignment for $(expr.head) not supported"))
        else
            show_expr(io,expr.args[1])
            print(io,ListPrint()>0 ? "=" : ":=")
            print(io,match(r"[^(=$)]",string(expr.head)).match*"(")
            show_expr(io,expr.args[1])
            print(io,",")
            show_expr(io,expr.args[2])
            print(io,")")
        end
    elseif expr.head == :for
        print(io,"for ")
        show_expr(io,expr.args[1])
        show_expr(io,expr.args[2])
    elseif expr.head == :block
        lxpr = linefilter!(expr)
        if length(lxpr.args) == 1
            show_expr(io,lxpr.args[1])
        else
            print(io,"begin ")
            show_expr(io,lxpr.args[1])
            for k ∈ 2:length(lxpr.args[2:end])+1
                print(io,";")
                show_expr(io,lxpr.args[k])
            end
            print(io," end")
        end
    elseif expr.head == :function
        print(io,"procedure ")
        show_expr(io,expr.args[1])
        print(io,";")
        show_expr(io,expr.args[2])
    elseif expr.head == :return
        print(io,"return ")
        show_expr(io,expr.args[1])
    elseif expr.head == :(::)
        show_expr(io,expr.args[1])
    elseif expr.head == :macrocall
        if expr.args[1] == Symbol("@big_str") || expr.args[1] == Symbol("@int128_str")
            print(io,expr.args[end])
        else
            throw(ReduceError("Macro $(expr.args[1]) block structure not supported\n\n$expr"))
        end
    elseif expr.head == :(:)
        show_expr(io,expr.args[1])
        print(io,":")
        show_expr(io,expr.args[2])
        print(io," ")
    elseif expr.head == :vcat
        print(io,"mat((")
        for i ∈ 1:length(expr.args)-1
            show_expr(io,expr.args[i])
            print(io,"),(")
        end
        show_expr(io,expr.args[end])
        print(io,"))")
    elseif expr.head == :row
        for i ∈ 1:length(expr.args)-1
            show_expr(io,expr.args[i])
            print(io,",")
        end
        print(io,expr.args[end])
    elseif expr.head == :tuple
        ListPrint(ListPrint()+1)
        print(io,"{")
        l = length(expr.args)
        for i ∈ 1:l
            show_expr(io,expr.args[i])
            i ≠ l && print(io,",")
        end
        print(io,"}")
        ListPrint(ListPrint()-1)
    elseif expr.head == :.
        if expr.args[1] == :MathConstants
            print(io,JSymReplace(string(expr)))
        else
            throw(ReduceError("$(expr.args[1]) module scope not supported"))
        end
    elseif expr.head == :line; nothing
    else
        throw(ReduceError("Nested :$(expr.head) block structure not supported\n\n$expr"))
    end
end

const infix_ops = ["+", "-", "*", "/", "**", "^"]
isinfix(args) = replace(args,' '=>"") in infix_ops

function show_expr(io::IO, ex)
    ((ex == :nothing) | (typeof(ex) == LineNumberNode)) && (return nothing)
    if typeof(ex) <: AbstractFloat && isinf(ex)
        print(io,(r > 0 ? "" : "-")*"infinity")
        return nothing
    elseif typeof(ex) <: Irrational
        print(io,unparse_irrational(ex))
        return nothing
    end
    if typeof(ex) <: Matrix
        print(io, "mat(")
        li = size(ex)[1]
        lj = size(ex)[2]
        for i ∈ 1:li
            print(io, "(")
            for j ∈ 1:lj-1
                show_expr(io,ex[i,j])
                print(io,",")
            end
            show_expr(io,ex[i,lj])
            print(io,")")
            i ≠ li && print(io,",")
        end
        print(io,")")
    elseif typeof(ex) <: Vector
        print(io,"mat(")
        l = length(ex)
        for i ∈ 1:l
            print(io,"(")
            show_expr(io,ex[i])
            print(io,")")
            i ≠ l && print(io,",")
        end
        print(io,")")
    elseif typeof(ex) <: Adjoint
        print(io,"mat((")
        l = length(ex)
        for i ∈ 1:l
            show_expr(io,ex[i])
            i ≠ l && print(io,",")
        end
        print(io,"))")
    elseif typeof(ex) <: Tuple
        ListPrint(ListPrint()+1)
        print(io,"{")
        l = length(ex)
        for i ∈ 1:l
            show_expr(io,ex[i])
            i ≠ l && print(io,",")
        end
        print(io,"}")
        ListPrint(ListPrint()-1)
    elseif typeof(ex) <: Pair
        show_expr(io,ex[1])
        print(io," => ")
        if typeof(ex[2]) <: Array
            show_expr(io,ex[2][1])
            for k ∈ 2:length(ex[2])
                print(io, k ≠ 1 ? " and " : " when ")
                show_expr(io, ex[2][k])
            end
        else
            show_expr(io,ex[2])
        end
    elseif typeof(ex) <: Array
        if length(size(ex)) > 2
            throw(ReduceError("parsing of $(typeof(ex)) not supported."))
        end
    else
        edit = IOBuffer()
        print(edit, ex)
        print(io, edit |> take! |> String |> JSymReplace)
    end
end

@inline function unparse_irrational(ex::T) where T <: Irrational
    if ex == ℯ
        return "e"
    elseif ex == π
        return "pi"
    else
        throw(ReduceError("$(typeof(ex)) not yet supported"))
    end
end

function unparse(expr::Expr)
    str = Array{String,1}(undef,0)
    io = IOBuffer()
    if expr.head == :block
        for line ∈ expr.args # block structure
            show_expr(io,line)
            push!(str,String(take!(io)))
        end
        return rtrim(str)
    else
        show_expr(io, expr)
        return push!(str,String(take!(io)))
    end
end

"""
    unfoldgen(::Symbol,::Symbol)

Parser generator that outputs code to walk and manipulate Julia expressions
"""
function unfoldgen(fun::Symbol,mode::Symbol)
    modefun = Symbol(:parse,"_",mode)
    fargs = if mode != :args
        [:(r::Union{<:Expr,<:Symbol})]
    else
        [:(r::Union{<:Expr,<:Symbol}),Expr(:...,:s)]
    end
    sargs = mode != :args ? [:(RExpr(r))] : [:(RExpr(r)),Expr(:...,:s)]
    return quote
        function $fun($(fargs...))
            if typeof(r) == Symbol
                convert(Expr,$fun($(sargs...)))
            else
                unfold(Symbol.($(string.([mode,fun])))...,$(fargs...))
            end
        end
    end
end

@noinline function unfold_expr(mode::Symbol, fun::Symbol, expr::Expr, s...; force=false)
    #expr = mode == :unary ? squash(ixpr) : ixpr
    force && return unfold_expr_force(mode,fun,expr,s...)
    if expr.head in [:call,:block,:(:)]
        if expr.args[1] == :(==) && fun == :solve
            return solve(RExpr(expr.args[2]),s...) |> parse
        else
            return unfold_expr_force(mode,fun,expr,s...)
        end
    elseif expr.head == :block
        return Expr(expr.head,unfold_expr.(mode,fun,expr.args,s...)...)
    elseif expr.head == :(:)
        return Expr(expr.head,unfold_expr_force.(mode,fun,expr.args,s...)...)
    elseif expr.head == :return
        return Expr(expr.head,unfold_expr.(mode,fun,expr.args,s...)...)
    elseif expr.head == :for
        return Expr(expr.head,expr.args[1],unfold_expr.(mode,fun,expr.args[2:end],s...)...)
    elseif occursin(r"=$",string(expr.head))
        if (typeof(expr.args[1]) == Expr) && (expr.args[1].head == :call)
            return Expr(expr.head,expr.args[1],unfold_expr_force.(mode,fun,expr.args[2:end],s...)...)
        else
            return Expr(expr.head,expr.args[1],unfold_expr(mode,fun,expr.args[2],s...;force=true))
        end
    elseif expr.head == :function
        return Expr(expr.head,expr.args[1],unfold_expr_force.(mode,fun,expr.args[2:end],s...)...)
    elseif expr.head == :(::)
        return Expr(:(::),unfold_expr(mode,fun,expr.args[1],s...),expr.args[2])
    elseif expr.head ∈ [:macrocall,:vcat,:row]
        return unfold_expr_force(mode,fun,expr,s...)
    elseif expr.head == :line
        return nothing
    else
        throw(ReduceError("Nested :$(expr.head) block structure not supported\n\n$expr"))
    end
end

function unfold_expr_force(mode::Symbol, fun::Symbol, ex, s...)
    out = nothing
    (typeof(ex) == LineNumberNode) && (return ex)
    if mode == :unary
        out = parse_unary(string(fun),RExpr(ex);be=0)
    elseif mode == :switch
        out = parse_switch(string(fun),RExpr(ex);be=0)
    elseif mode ==:args
        out = parse_args(string(fun),RExpr(ex),s...;be=0)
    else
        throw(ReduceError("Parse mode not supported."))
    end
    fun ∈ switchtex ? out : typeof(ex) <: Matrix ? mat(out) : convert(Expr, out)
end

function unfold_expr(mode::Symbol, fun::Symbol, ex, s...; force=true)
    typeof(ex) in [Nothing,LineNumberNode] ? ex : unfold_expr_force(mode,fun,ex,s...)
end

function unfold(mode::Symbol,fun::Symbol,expr::Expr,s...)
    #expr = mode == :unary ? squash(ixpr) : ixpr
    if expr.head == :block
        out = Any[]
        for line ∈ expr.args # block structure
            push!(out,unfold_expr(mode,fun,line,s...))
        end
        return Expr(:block,out...)
    else
        return unfold_expr(mode,fun,expr,s...)
    end
end

function mat(expr)
    if typeof(expr) == Expr && expr.head == :vcat
        if typeof(expr.args[1]) == Expr && expr.args[1].head == :row
            rows = length(expr.args)
            cols = length(expr.args[1].args)
            out = Array{Any,2}(undef,rows,cols)
            for k ∈ 1:rows
                for l ∈ 1:cols
                    out[k,l] = expr.args[k].args[l]
                end
            end
            return out
        else
            rows = length(expr.args)
            out = Array{Any,1}(undef,rows)
            for k ∈ 1:rows
                out[k] = expr.args[k]
            end
            return out
        end
    else
        return expr
    end
end

mat(expr,original...) = |(typeof.(original)...) <: Matrix ? mat(expr) : expr
