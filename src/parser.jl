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
                if contains(sh[en], "procedure")
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
                elseif contains(sh[en], "begin")
                    js = join(split(sexpr[h],"begin")[2:end],"begin")
                    ep = Array{$arty,1}(0)
                    sh1 = split(js,r"[ ]+")
                    c = sum(sh1.=="begin")-sum(sh1.=="end")
                    flag = c ≥ 0
                    c ≤ -1 && (js = join(split(js,"end")[1:end+c],"end"))
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
                        c ≤ -1 && (js = join(split(js,"end")[1:end+c],"end"))
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
                elseif contains(sh[en],"return")
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
            else
                :(return RExpr(nsr))
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

parsegen(:parse,:expr) |> eval

@doc """
    parse(r::RExpr)

Parse a Reduce expression into a Julia expression

# Examples
```julia-repl
julia> parse(R\"sin(i*x)\")
:(sinh(x) * im)
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
    elseif expr.head == :macrocall
        if expr.args[1] == Symbol("@big_str")
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

const infix_ops = ["+", "-", "*", "/", "**"]
isinfix(args) = args in infix_ops

#show_expr(io::IO, ex) = print(io, ex |> string |> JSymReplace)
function show_expr(io::IO, ex)
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
