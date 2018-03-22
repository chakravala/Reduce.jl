#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

"""
REDUCE begin and end marker counter for parsegen
"""
@noinline function bematch(js,sexpr,h,iter,state)
    sh = split(js,r"[ ]+")
    y = h
    c = sum(sh.=="begin")-sum(sh.=="end")
    flag = c > 0
    while !done(iter,state) & flag
        (y,state) = next(iter, state)
        sh2 = split(sexpr[y],r"[ ]+")
        c += sum(sh2.=="begin")-sum(sh2.=="end")
        flag = c > 0
    end
    return (y,state)
end

"""
REDUCE parenthesis marker counter for parsegen
"""
@noinline function pmatch(js,sexpr,h,iter,state)
    y = h
    c = count(z->z=='(',js)-count(z->z==')',js)
    flag = c > 0
    while !done(iter,state) & flag
        (y,state) = next(iter, state)
        c += count(z->z=='(',sexpr[y])-count(z->z==')',sexpr[y])
        flag = c > 0
    end
    return (y,state)
end

const prefix = r"(?<!\))(([A-Za-z_][A-Za-z_0-9]*)|([\^+\/-])|([*]{1,2})|(- ))(?=\()"
const parens = r"\(((?>[^\(\)]+)|(?R))*\)"
const infix1 = r"^(([\^\+\/])|([*]{1,2})|( -)|( \+)|( [*]{1,2})|( /)|( \^))"
const infix2 = r"(([\^+\/])|([*]{1,2}))$"
const assign = r"^([A-Za-z_][A-Za-z_0-9]*)(:=)"

for mode ∈ [:expr,:unary,:switch,:calculus]
    rfun = Symbol(:r,"_",mode)
    modefun = Symbol(:parse,"_",mode)
    argfun = Symbol(:args,"_",mode)
    arty = (mode == :expr) ? :Any : :(Compat.String)
    exec = if mode == :expr
        :(Meta.parse(RSymReplace(js)))
    elseif mode == :unary
        :(fun * "($js)" |> RExpr |> rcall)
    elseif mode == :switch
        :(rcall("$js" |> RExpr, fun))
    elseif mode == :calculus
        :(fun * "($js,$(join(s,',')))" |> RExpr |> rcall)
    end
    mode != :calculus ? (fargs = [:(r::RExpr)]) : (fargs = [:(r::RExpr),Expr(:...,:sr)])
    mode != :calculus ? (aargs = [:be]) : (aargs = [:be,Expr(:...,:s)])
    quote
        @noinline function $modefun(fun::String,$(fargs...);be=0)
            nsr = $arty[]
            $(if mode == :calculus
                quote
                    s = Array{Compat.String,1}()
                    for rs ∈ sr
                        push!(s,rs |> RExpr |> string)
                    end
                end
            end)
            sexpr = split(r).str
            iter = 1:length(sexpr)
            state = start(iter); #show(sexpr)
            while !done(iter,state)
                (h,state) = next(iter, state)
                sh = split(sexpr[h],r"[ ]+")
                en = 1
                isempty(replace(sh[en]," "=>"")) && (en = 2); #show(sh[en])
                if contains(sh[en],r"^procedure")
                    js = join(split(sexpr[h],"procedure")[2:end],"procedure")
                    (h,state) = next(iter, state)
                    y = h
                    (h,state) = bematch(sexpr[h],sexpr,h,iter,state)
                    $(mode != :expr ? :(push!(nsr,Compat.String("procedure "*js))) : :(nothing))
                    $(if mode == :expr
                        :(push!(nsr,Expr(:function,Meta.parse(js),$rfun(fun,sexpr[y:h];be=be))))
                    elseif mode == :calculus
                        :(push!(nsr,$rfun(fun,sexpr[y:h],s;be=be) |> string))
                    else
                        :(push!(nsr,$rfun(fun,sexpr[y:h];be=be) |> string))
                    end)
                elseif contains(sh[en],r"^begin")
                    js = join(split(sexpr[h],"begin")[2:end],"begin")
                    ep = Array{$arty,1}(0)
                    sh1 = split(js,r"[ ]+")
                    c = sum(sh1.=="begin")-sum(sh1.=="end")
                    flag = c ≥ 0
                    !flag && (js = join(split(js,"end")[1:end+c],"end"))
                    y = h
                    (h,state) = bematch(js,sexpr,h,iter,state)
                    $(mode != :expr ? :(push!(nsr,Compat.String("begin "*js))) : :(nothing))
                    $(if mode == :expr
                        :(push!(ep,$rfun(fun,vcat(js,sexpr[y+1:h]...);be=be+1)))
                    elseif mode == :calculus
                        :(push!(ep,$rfun(fun,vcat(js,sexpr[y+1:h]...),s;be=be+1) |> string))
                    else
                        :(push!(ep,$rfun(fun,vcat(js,sexpr[y+1:h]...);be=be+1) |> string))
                    end)
                    ep[1] == nothing && shift!(ep)
                    while !done(iter,state) & flag
                        (h,state) = next(iter, state)
                        cQ = c
                        js = sexpr[h]
                        sh2 = split(js,r"[ ]+")
                        c += sum(sh2.=="begin")-sum(sh2.=="end")
                        if c ≤ -1
                            js = join(split(js,"end")[1:end+c],"end")
                            flag = false
                        end
                        y = h
                        (h,state) = bematch(js,sexpr,h,iter,state)
                        $(if mode == :expr
                            :(epr = $rfun(fun,vcat(js,sexpr[y+1:h]...);be=cQ))
                        elseif mode == :calculus
                            :(epr = $rfun(fun,vcat(js,sexpr[y+1:h]...),s;be=cQ) |> string)
                        else
                            :(epr = $rfun(fun,vcat(js,sexpr[y+1:h]...);be=cQ) |> string)
                        end)
                        epr ≠ nothing && push!(ep,epr)
                    end
                    push!(nsr,$((mode == :expr) ? :(Expr(:block,ep...)) : Expr(:...,:ep)))
                    $(mode != :expr ? :(push!(nsr,Compat.String("end"))) : :(nothing))
                elseif contains(sh[en],r"^return")
                    js = join(split(sexpr[h],"return")[2:end],"return")
                    y = h
                    (h,state) = bematch(js,sexpr,h,iter,state)
                    $(if mode == :expr
                        :(rp = $rfun(fun,vcat(js,sexpr[y+1:h]...);be=be))
                    elseif mode == :calculus
                        :(rp = $rfun(fun,vcat(js,sexpr[y+1:h]...),s;be=be) |> string)
                    else
                        :(rp = $rfun(fun,vcat(js,sexpr[y+1:h]...);be=be) |> string)
                    end)
                    $(mode != :expr ? :(push!(nsr,"return "*rp)) : :(push!(nsr,Expr(:return,rp))))
                elseif contains(sh[en],assign)
                    sp = split(sexpr[h], ":=",limit=2)
                    $(if mode == :expr
                        :(push!(nsr,Expr(:(=),Meta.parse(sp[1]),$rfun(fun,sp[2];be=be))))
                    elseif mode == :calculus
                        :(push!(nsr, Compat.String(sp[1]) * ":=" * string($rfun(fun,sp[2] |> Compat.String |> RExpr,s;be=be))))
                    else
                        :(push!(nsr, Compat.String(sp[1]) * ":=" * string($rfun(fun,sp[2] |> Compat.String |> RExpr;be=be))))
                    end)
                elseif contains(sh[en],"for")
                    throw(ReduceError("for block parsing not supported"))
                elseif contains($((mode == :expr) ? :(sexpr[h]) : :("")),prefix)
                    $(if mode == :expr; quote
                    ts = sexpr[h]
                    mp = Array{Compat.String,1}(0)
                    c = count(z->z=='(',ts)-count(z->z==')',ts)
                    flag = c ≥ 0
                    !flag && (ts = join(split(ts,')')[1:end+c],')'))
                    y = h
                    (h,state) = pmatch(ts,sexpr,h,iter,state)
                    push!(mp,ts,sexpr[y+1:h]...)
                    mp[1] == nothing && shift!(mp)
                    while !done(iter,state) & flag
                        (h,state) = next(iter, state)
                        cQ = c
                        ts = sexpr[h]
                        c += count(z->z=='(',ts)-count(z->z==')',ts)
                        if c ≤ -1
                            ts = join(split(ts,')')[1:end+c],')')
                            flag = false
                        end
                        y = h
                        (h,state) = pmatch(ts,sexpr,h,iter,state)
                        mpr = vcat(ts,sexpr[y+1:h]...)
                        mpr ≠ nothing && push!(mp,mpr...)
                    end
                    smp = join(mp,";\n")
                    qr = ""
                    while smp ≠ ""
                        args = Array{$arty,1}(0)
                        if contains(smp,r"^\(")
                            push!(args,$(if mode == :expr
                                :($rfun(fun,match(parens,smp).match[2:end-1];be=be))
                            elseif mode == :calculus
                                :($rfun(fun,match(parens,smp).match[2:end-1],s;be=be) |> string)
                            else
                                :($rfun(fun,match(parens,smp).match[2:end-1];be=be) |> string)
                            end))
                            smp = split(smp,parens;limit=2)[end]
                            qr = qr * "($(args...))"
                            if !contains(smp,prefix)
                                $(if mode == :expr; quote
                                    if contains(smp,infix1)
                                        qr = qr * RSymReplace(match(infix1,smp).match) * RSymReplace(split(smp,infix1)[end])
                                        smp = ""
                                    else
                                        qr = qr * RSymReplace(smp)
                                        smp = ""
                                    end; end
                                else
                                    :(qr = qr * smp; smp = "")
                                end)
                            elseif contains(smp,infix1)
                                qr = qr * RSymReplace(match(infix1,smp).match)
                                smp = split(smp,infix1)[end]
                            end
                            continue
                        end
                        pf = match(prefix,smp).match |> String
                        sp = split(smp,prefix;limit=2)
                        $(if mode == :expr; quote 
                            if contains(sp[1],infix2)
                                rq = split(sp[1],infix2)[1]
                                if contains(rq,infix1)
                                    rq = RSymReplace(match(infix1,rq).match) * RSymReplace(split(rq,infix1)[end])
                                else
                                    rq = RSymReplace(rq)
                                end
                                qr = qr * rq * RSymReplace(match(infix2,sp[1]).match)
                            elseif contains(sp[1],infix1)
                                qr = qr * RSymReplace(match(infix1,sp[1]).match) * RSymReplace(split(sp[1],infix1)[end])
                            else
                                qr = qr * RSymReplace(sp[1])
                            end; end
                        else
                            :(qr = qr * sp[1])
                        end)
                        smp = split(sp[2],parens;limit=2)[end]
                        lsm = match(parens,sp[2]).match[2:end-1]
                        if pf == "mat"
                            mt = matchall(parens,lsm)
                            for row ∈ mt
                                elm = split(row[2:end-1],',')
                                push!(args,$(if mode == :expr
                                    :(Expr(:row,$argfun(fun,$(string(mode)),elm,be)...))
                                elseif mode == :calculus
                                    :($argfun(fun,$(string(mode)),elm,be,s) |> string)
                                else
                                    :($argfun(fun,$(string(mode)),elm,be) |> string)
                                end))
                            end
                            qr = qr * $(if mode == :expr
                                :(Expr(:vcat,args...) |> string)
                            else
                                :("$pf($(join(args,',')))")
                            end)
                        else
                            ls = split(lsm,',')
                            push!(args,$(if mode == :expr
                                :($argfun(fun,$(string(mode)),ls,be))
                            elseif mode == :calculus
                                :($argfun(fun,$(string(mode)),ls,be,s) |> string)
                            else
                                :($argfun(fun,$(string(mode)),ls,be) |> string)
                            end)...)
                            rq = "$(RSymReplace(pf))($(join(args,',')))"
                            qr = qr * $(if mode == :expr
                                :(((isinfix(pf) && length(args) == 1) ? rq : "($rq)"))
                            else
                                :("$pf($(join(args,',')))")
                            end)
                        end
                        !contains(smp,prefix) && ($(if mode == :expr; quote
                            if contains(smp,infix1)
                                qr = qr * RSymReplace(match(infix1,smp).match) * RSymReplace(split(smp,infix1)[end])
                                smp = ""
                            else
                                qr = qr * RSymReplace(smp)
                                smp = ""
                            end; end
                        else
                            :(qr = qr * smp; smp = "")
                        end))
                    end
                    push!(nsr,$((mode == :expr) ? :(u = "("*qr*")" |> Meta.parse |> linefilter) : :qr))
                    end; else; :(nothing); end)
                elseif contains(sh[en],"end")
                    nothing
                elseif isempty(sh[en])
                    nothing
                elseif contains(sexpr[h],":")
                    sp = split(sexpr[h],":")
                    $(if mode == :expr
                        :(push!(nsr,Expr(:(:),$rfun(fun,sp[1];be=be),$rfun(fun,sp[2];be=be))))
                    elseif mode == :calculus
                        :(push!(nsr,($rfun(fun,sp[1];be=be)|>string)*":"*($rfun(fun,sp[2],s;be=be)|>string)))
                    else
                        :(push!(nsr,($rfun(fun,sp[1];be=be)|>string)*":"*($rfun(fun,sp[2];be=be)|>string)))
                    end)
                else
                    js=sexpr[h]
                    se=sum(sh.=="end")
                    0<se≤be ? (js=replace(js,"end","")) :
                        (se>be && (js=join(split(js,"end")[1:end-be],"end")))
                    push!(nsr, $exec)
                end
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
        $(if mode == :calculus
            quote
                $rfun(fun::String,r::Array{Compat.String,1},s;be=0) = $modefun(fun,RExpr(r),s...;be=be)
                $rfun(fun::String,r,s;be=0) = $modefun(fun,r |> Compat.String |> RExpr,s...;be=be)
            end
        else
            quote
                $rfun(fun::String,r::Array{Compat.String,1};be=0) = $modefun(fun,RExpr(r);be=be)
                $rfun(fun::String,r;be=0) = $modefun(fun,r |> Compat.String |> RExpr;be=be)
            end
        end)
        @noinline function $argfun(fun::String,mod::String,ls::Array{SubString{String},1},$(aargs...))
            mode = Symbol(mod)
            args = Array{$arty,1}(0)
            lsi = 1:length(ls)
            lss = start(lsi)
            while !done(lsi,lss)
                (lsh,lss) = next(lsi, lss)
                if contains(ls[lsh],r"^begin")
                    js = join(split(ls[lsh],"begin")[2:end],"begin")
                    ep = Array{$arty,1}(0)
                    sh1 = split(js,r"[ ]+")
                    c = sum(sh1.=="begin")-sum(sh1.=="end")
                    flag = c ≥ 0
                    !flag && (js = join(split(js,"end")[1:end+c],"end"))
                    lsy = lsh
                    (lsh,lss) = bematch(js,ls,lsh,lsi,lss)
                    push!(ep,js,ls[lsy+1:lsh]...)
                    ep[1] == nothing && shift!(ep)
                    while !done(lsi,lss) & flag
                        (lsh,lss) = next(lsi, lss)
                        cQ = c
                        js = ls[lsh]
                        sh2 = split(js,r"[ ]+")
                        c += sum(sh2.=="begin")-sum(sh2.=="end")
                        if c ≤ -1
                            js = join(split(js,"end")[1:end+c],"end")
                            flag = false
                        end
                        lsy = lsh
                        (lsh,lss) = bematch(js,ls,lsh,lsi,lss)
                        epr = vcat(js,ls[lsy+1:lsh]...)
                        epr ≠ nothing && push!(ep,epr...)
                    end
                    sep = "begin "*join(ep,',')*" end"
                    push!(args,$(if mode == :expr
                        :($rfun(fun,sep;be=be))
                    elseif mode == :calculus
                        :($rfun(fun,sep,s;be=be) |> string)
                    else
                        :($rfun(fun,sep;be=be) |> string)
                    end))
                elseif contains(ls[lsh],prefix)
                    js = ls[lsh]
                    ep = Array{$arty,1}(0)
                    c = count(z->z=='(',js)-count(z->z==')',js)-1
                    flag = c ≥ -1
                    !flag && (js = join(split(js,')')[1:end+c],')'))
                    lsy = lsh
                    (lsh,lss) = pmatch(js,ls,lsh,lsi,lss)
                    push!(ep,js,ls[lsy+1:lsh]...)
                    ep[1] == nothing && shift!(ep)
                    sep = join(ep,',')
                    push!(args,$(if mode == :expr
                        :($rfun(fun,sep;be=be))
                    elseif mode == :calculus
                        :($rfun(fun,sep,s;be=be) |> string)
                    else
                        :($rfun(fun,sep;be=be) |> string)
                    end))
                else
                    push!(args,$(if mode == :expr
                        :($rfun(fun,ls[lsh];be=be))
                    elseif mode == :calculus
                        :($rfun(fun,ls[lsh],s;be=be) |> string)
                    else
                        :($rfun(fun,ls[lsh];be=be) |> string)
                    end))
                end
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
    modefun = Symbol(:parse,"_",mode)
    mode != :calculus ? (fargs = [:(r::RExpr)]) : (fargs = [:(r::RExpr),Expr(:...,:s)])
    return :($fun($(fargs...);be=0) = $modefun($(string(fun)),$(fargs...);be=0))
end

"""
    linefilter(::Expr)

Recursively filters out :line blocks from Expr objects
"""
function linefilter(e::Expr)
    total = length(e.args)
    i = 0
    while i < total
        i += 1
        if e.args[i] |> typeof == Expr
            if e.args[i].head == :line
                deleteat!(e.args,i)
                total -= 1
                i -= 1
            else
                e.args[i] = linefilter(e.args[i])
            end
        end
    end
    return e
end

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
        i ≠ endof(a) ? print(io,",") : print(io,")")
    end
end

function show_expr(io::IO, expr::Expr) # recursively unparse Julia expression
    if expr.head == :call
        show_expr(io, expr.args[1])
        print_args(io,expr.args[2:end])
    elseif expr.head == :(=)
        show_expr(io,expr.args[1])
        print(io,":=")
        show_expr(io,expr.args[2])
    elseif expr.head == :for
        print(io,"for ")
        show_expr(io,expr.args[1])
        show_expr(io,expr.args[2])
    elseif expr.head == :block
        print(io,"begin ")
        show_expr(io,expr.args[1])
        for k ∈ 2:length(expr.args[2:end])+1
            print(io,";")
            show_expr(io,expr.args[k])
        end
        print(io," end")
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
            print(io,expr.args[2])
        else
            throw(ReduceError("Macro $(expr.args[1]) block structure not supported"))
        end
    elseif expr.head == :(:)
        show_expr(io,expr.args[1])
        print(io,":")
        show_expr(io,expr.args[2])
        print(io," ")
    elseif expr.head == :vcat
        print(io,"mat(")
        for i ∈ 1:length(expr.args)-1
            show_expr(io,expr.args[i])
            print(io,",")
        end
        show_expr(io,expr.args[end])
        print(io,")")
    elseif expr.head == :row
        print(io,"(")
        for i ∈ 1:length(expr.args)-1
            show_expr(io,expr.args[i])
            print(io,",")
        end
        print(io,expr.args[end])
        print(io,")")
    elseif expr.head == :line; nothing
    else
        throw(ReduceError("Nested :$(expr.head) block structure not supported"))
    end
end

const infix_ops = ["+", "-", "*", "/", "**", "^"]
isinfix(args) = replace(args,' '=>"") in infix_ops

#show_expr(io::IO, ex) = print(io, ex |> string |> JSymReplace)
function show_expr(io::IO, ex)
    ex == :nothing && (return nothing)
    if typeof(ex) <: AbstractFloat && isinf(ex)
        println((r > 0 ? "" : "-")*"infinity")
        return nothing
    end
    edit = IOBuffer()
    if typeof(ex) <: Matrix
        print(io, "mat(")
        li = size(ex)[1]
        lj = size(ex)[2]
        for i ∈ 1:li
            print(io, "(")
            for j ∈ 1:lj-1
                print(io,ex[i,j])
                print(io,",")
            end
            print(io,ex[i,lj])
            print(io,")")
            i ≠ li && print(io,",")
        end
        print(io,")")
    else
        print(edit, ex)
        print(io, edit |> String |> JSymReplace)
    end
end

function unparse(expr::Expr)
    str = Array{Compat.String,1}(0)
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

Parser generator that outputs code to walk and manipulate REDUCE expressions
"""
function unfoldgen(fun::Symbol,mode::Symbol)
    modefun = Symbol(:parse,"_",mode)
    fargs = if mode != :calculus
        [:(r::Union{<:Expr,<:Symbol})]
    else
        [:(r::Union{<:Expr,<:Symbol}),Expr(:...,:s)]
    end
    sargs = mode != :calculus ? [:(RExpr(r))] : [:(RExpr(r)),Expr(:...,:s)]
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

function unfold_expr(mode::Symbol, fun::Symbol, expr::Expr, s...; force=false)
    force && return unfold_expr_force(mode,fun,expr,s...)
    if expr.head in [:call,:block,:(:)]
        unfold_expr_force(mode,fun,expr,s...)
    elseif expr.head == :block
        return Expr(expr.head,unfold_expr.(mode,fun,expr.args,s...)...)
    elseif expr.head == :(:)
        return Expr(expr.head,unfold_expr_force.(mode,fun,expr.args,s...)...)
    elseif expr.head == :return
        return Expr(expr.head,unfold_expr.(mode,fun,expr.args,s...)...)
    elseif expr.head == :for
        return Expr(expr.head,expr.args[1],unfold_expr.(mode,fun,expr.args[2:end],s...)...)
    elseif expr.head == :(=)
        if (typeof(expr.args[1]) == Expr) && (expr.args[1].head == :call)
            return Expr(expr.head,expr.args[1],unfold_expr_force.(mode,fun,expr.args[2:end],s...)...)
        else
            return Expr(expr.head,expr.args[1],unfold_expr(mode,fun,expr.args[2],s...;force=true)...)
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
        throw(ReduceError("Nested :$(expr.head) block structure not supported"))
    end
end

function unfold_expr_force(mode::Symbol, fun::Symbol, ex, s...)
    out = nothing
    if mode == :unary
        out = parse_unary(string(fun),RExpr(ex);be=0)
    elseif mode == :switch
        out = parse_switch(string(fun),RExpr(ex);be=0)
    elseif mode ==:calculus
        out = parse_calculus(string(fun),RExpr(ex),s...;be=0)
    else
        throw(ReduceError("Parse mode not supported."))
    end
    return (fun ∈ switchtex) ? out : convert(Expr, out)
end

function unfold_expr(mode::Symbol, fun::Symbol, ex, s...; force=true)
    typeof(ex) in [Void] ? ex : unfold_expr_force(mode,fun,ex,s...)
end

function unfold(mode::Symbol,fun::Symbol,expr::Expr,s...)
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
