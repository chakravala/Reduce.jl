#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

"""
REDUCE begin and end marker counter for parsegen
"""
function bematch(js,sexpr,h,iter,state)
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
function pmatch(js,sexpr,h,iter,state)
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


"""
    parsegen(::Symbol,::Symbol)

Parser generator that outputs code to walk and manipulate REDUCE expressions
"""
function parsegen(fun::Symbol,mode::Symbol)
    rfun = Symbol(:r,fun)
    arty = (mode == :expr) ? :Any : :(Compat.String)
    exec = if mode == :expr
        :(parse(RSymReplace(js)))
    elseif mode == :unary
        :($(string(fun)) * "($js)" |> RExpr |> rcall)
    elseif mode == :switch
        :(rcall("$js" |> RExpr, $(string(fun))))
    elseif mode == :calculus
        :($(string(fun)) * "($js,$(join(s,',')))" |> RExpr |> rcall)
    end
    mode != :calculus ? (args = [:(r::RExpr)]) : (args = [:(r::RExpr),Expr(:...,:s)])
    return quote
        function $fun($(args...);be=0)
            nsr = $arty[]
            sexpr = split(r).str
            iter = 1:length(sexpr)
            state = start(iter); #show(sexpr)
            while !done(iter,state)
                (h,state) = next(iter, state)
                sh = split(sexpr[h],r"[ ]+")
                en = 1
                isempty(replace(sh[en]," ","")) && (en = 2); #show(sh[en])
                if ismatch(r"^procedure",sh[en])
                    js = join(split(sexpr[h],"procedure")[2:end],"procedure")
                    (h,state) = next(iter, state)
                    y = h
                    (h,state) = bematch(sexpr[h],sexpr,h,iter,state)
                    $(mode != :expr ? :(push!(nsr,Compat.String("procedure "*js))) : :(nothing))
                    $(if mode == :expr
                        :(push!(nsr,Expr(:function,parse(js),rparse(sexpr[y:h];be=be))))
                    elseif mode == :calculus
                        :(push!(nsr,$rfun(sexpr[y:h],s;be=be) |> string))
                    else
                        :(push!(nsr,$rfun(sexpr[y:h];be=be) |> string))
                    end)
                elseif ismatch(r"^begin",sh[en])
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
                        :(push!(ep,$rfun(vcat(js,sexpr[y+1:h]...);be=be+1)))
                    elseif mode == :calculus
                        :(push!(ep,$rfun(vcat(js,sexpr[y+1:h]...),s;be=be+1) |> string))
                    else
                        :(push!(ep,$rfun(vcat(js,sexpr[y+1:h]...);be=be+1) |> string))
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
                            :(epr = $rfun(vcat(js,sexpr[y+1:h]...);be=cQ))
                        elseif mode == :calculus
                            :(epr = $rfun(vcat(js,sexpr[y+1:h]...),s;be=cQ) |> string)
                        else
                            :(epr = $rfun(vcat(js,sexpr[y+1:h]...);be=cQ) |> string)
                        end)
                        epr ≠ nothing && push!(ep,epr)
                    end
                    push!(nsr,$((mode == :expr) ? :(Expr(:block,ep...)) : Expr(:...,:ep)))
                    $(mode != :expr ? :(push!(nsr,Compat.String("end"))) : :(nothing))
                elseif ismatch(r"^return",sh[en])
                    js = join(split(sexpr[h],"return")[2:end],"return")
                    y = h
                    (h,state) = bematch(js,sexpr,h,iter,state)
                    $(if mode == :expr
                        :(rp = $rfun(vcat(js,sexpr[y+1:h]...);be=be))
                    elseif mode == :calculus
                        :(rp = $rfun(vcat(js,sexpr[y+1:h]...),s;be=be) |> string)
                    else
                        :(rp = $rfun(vcat(js,sexpr[y+1:h]...);be=be) |> string)
                    end)
                    $(mode != :expr ? :(push!(nsr,"return "*rp)) : :(push!(nsr,Expr(:return,rp))))
                elseif contains(sh[en],"for")
                    throw(ReduceError("for block parsing not supported"))
                elseif ismatch(prefix,$((mode == :expr) ? :(sexpr[h]) : :("")))
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
                        if ismatch(r"^\(",smp)
                            push!(args,$(if mode == :expr
                                :($rfun(match(parens,smp).match[2:end-1];be=be))
                            elseif mode == :calculus
                                :($rfun(match(parens,smp).match[2:end-1],s;be=be) |> string)
                            else
                                :($rfun(match(parens,smp).match[2:end-1];be=be) |> string)
                            end))
                            smp = split(smp,parens;limit=2)[end]
                            qr = qr * "($(args...))"
                            if !ismatch(prefix,smp)
                                $(if mode == :expr; quote
                                    if ismatch(infix1,smp)
                                        qr = qr * RSymReplace(match(infix1,smp).match) * RSymReplace(split(smp,infix1)[end])
                                        smp = ""
                                    else
                                        qr = qr * RSymReplace(smp)
                                        smp = ""
                                    end; end
                                else
                                    :(qr = qr * smp; smp = "")
                                end)
                            elseif ismatch(infix1,smp)
                                qr = qr * RSymReplace(match(infix1,smp).match)
                                smp = split(smp,infix1)[end]
                            end
                            continue
                        end
                        pf = match(prefix,smp).match |> String
                        sp = split(smp,prefix;limit=2)
                        $(if mode == :expr; quote
                            if ismatch(infix2,sp[1])
                                rq = split(sp[1],infix2)[1]
                                if ismatch(infix1,rq)
                                    rq = RSymReplace(match(infix1,rq).match) * RSymReplace(split(rq,infix1)[end])
                                else
                                    rq = RSymReplace(rq)
                                end
                                qr = qr * rq * RSymReplace(match(infix2,sp[1]).match)
                            elseif ismatch(infix1,sp[1])
                                qr = qr * RSymReplace(match(infix1,sp[1]).match) * RSymReplace(split(sp[1],infix1)[end])
                            else
                                qr = qr * RSymReplace(sp[1])
                            end; end
                        else
                            :(qr = qr * sp[1])
                        end)
                        smp = split(sp[2],parens;limit=2)[end]
                        ls = split(match(parens,sp[2]).match[2:end-1],',')
                        lsi = 1:length(ls)
                        lss = start(lsi)
                        while !done(lsi,lss)
                            (lsh,lss) = next(lsi, lss)
                            if ismatch(r"^begin",ls[lsh])
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
                                    :($rfun(sep;be=be))
                                elseif mode == :calculus
                                    :($rfun(sep,s;be=be) |> string)
                                else
                                    :($rfun(sep;be=be) |> string)
                                end))
                            elseif ismatch(prefix,ls[lsh])
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
                                    :($rfun(sep;be=be))
                                elseif mode == :calculus
                                    :($rfun(sep,s;be=be) |> string)
                                else
                                    :($rfun(sep;be=be) |> string)
                                end))
                            else
                                push!(args,$(if mode == :expr
                                    :($rfun(ls[lsh];be=be))
                                elseif mode == :calculus
                                    :($rfun(ls[lsh],s;be=be) |> string)
                                else
                                    :($rfun(ls[lsh];be=be) |> string)
                                end))
                            end
                        end
                        rq = "$(RSymReplace(pf))($(join(args,',')))"
                        qr = qr * $(if mode == :expr
                            :(((isinfix(pf) && length(args) == 1) ? rq : "($rq)"))
                        else
                            :("$pf($(join(args,',')))")
                        end)
                        !ismatch(prefix,smp) && ($(if mode == :expr; quote
                            if ismatch(infix1,smp)
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
                    push!(nsr,$((mode == :expr) ? :(u = "("*qr*")" |> parse |> linefilter) : :qr))
                    end; else; :(nothing); end)
                elseif contains(sh[en],"end")
                    nothing
                elseif isempty(sh[en])
                    nothing
                elseif contains(sexpr[h], ":=")
                    sp = split(sexpr[h], ":=")
                    $(if mode == :expr
                        :(push!(nsr,Expr(:(=),parse(sp[1]),rparse(sp[2];be=be))))
                    elseif mode == :calculus
                        :(push!(nsr, Compat.String(sp[1]) * ":=" * string($rfun(sp[2] |> Compat.String |> RExpr,s;be=be))))
                    else
                        :(push!(nsr, Compat.String(sp[1]) * ":=" * string($rfun(sp[2] |> Compat.String |> RExpr;be=be))))
                    end)
                elseif contains(sexpr[h],":")
                    sp = split(sexpr[h],":")
                    $(if mode == :expr
                        :(push!(nsr,Expr(:(:),rparse(sp[1];be=be),rparse(sp[2];be=be))))
                    elseif mode == :calculus
                        :(push!(nsr,($rfun(sp[1];be=be)|>string)*":"*($rfun(sp[2],s;be=be)|>string)))
                    else
                        :(push!(nsr,($rfun(sp[1];be=be)|>string)*":"*($rfun(sp[2];be=be)|>string)))
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
            elseif fun in [:nat,:latex]
                :(return nsr |> RExpr |> split |> string)
            else
                :(return nsr |> RExpr |> split)
            end)
        end
        $(if mode == :calculus
            quote
                $rfun(r::Array{Compat.String,1},s;be=0) = $fun(RExpr(r),s...;be=be)
                $rfun(r,s;be=0) = $fun(r |> Compat.String |> RExpr,s...;be=be)
            end
        else
            quote
                $rfun(r::Array{Compat.String,1};be=0) = $fun(RExpr(r);be=be)
                $rfun(r;be=0) = $fun(r |> Compat.String |> RExpr;be=be)
            end
        end)
    end
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
    parse(r::RExpr)

Parse a Reduce expression into a Julia expression

# Examples
```julia-repl
julia> parse(R\"sin(i*x)\")
:(sin(im * x))
```
""" parse

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
    elseif expr.head == :line; nothing
    else
        throw(ReduceError("Nested :$(expr.head) block structure not supported"))
    end
end

const infix_ops = ["+", "-", "*", "/", "**", "^"]
isinfix(args) = replace(args,' ',"") in infix_ops

#show_expr(io::IO, ex) = print(io, ex |> string |> JSymReplace)
function show_expr(io::IO, ex)
    ex == :nothing && (return nothing)
    edit = IOBuffer()
    print(edit, ex)
    print(io, edit |> String |> JSymReplace)
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
        return push!(str,String(io))
    end
end
