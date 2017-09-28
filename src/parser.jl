#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

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

function parsegen(fun::Symbol,mode::Symbol)
    rfun = Symbol(:r,fun)
    arty = (mode == :expr) ? :Any : :(Compat.String)
    exec = if mode == :expr
        :(parse(RSymReplace(js)))
    elseif mode == :unary
        :($(string(fun)) * "($js)" |> RExpr |> rcall)
    elseif mode == :switch
        :(rcall("$js" |> RExpr, $(string(fun))))
    end
    return quote
        function $fun(r::RExpr,be=0)
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
                    $(if mode == :expr
                        :(push!(nsr,Expr(:function,parse(js),rparse(sexpr[y:h],be))))
                    else
                        quote
                            push!(nsr,Compat.String("procedure "*js))
                            push!(nsr,$rfun(sexpr[y:h],be) |> string)
                        end
                    end)
                elseif contains(sh[en], "begin")
                    js = join(split(sexpr[h],"begin")[2:end],"begin")
                    ep = $arty[]
                    sh1 = split(js,r"[ ]+")
                    c = sum(sh1.=="begin")-sum(sh1.=="end")
                    flag = c ≥ 0
                    c ≤ -1 && (js = join(split(js,"end")[1:end+c],"end"))
                    y = h
                    (h,state) = bematch(js,sexpr,h,iter,state)
                    $(if mode == :expr
                        :(ep[1] = $rfun(vcat(js,sexpr[y+1:h]...),be+1))
                    else
                        :(ep[1] = $rfun(vcat(js,sexpr[y+1:h]...),be+1) |> string)
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
                            :(epr = $rfun(vcat(js,sexpr[y+1:h]...),cQ))
                        else
                            :(epr = $rfun(vcat(js,sexpr[y+1:h]...),cQ) |> string)
                        end)
                        epr ≠ nothing && push!(ep,epr)
                    end
                    push!(nsr,(mode == :expr) ? (Expr(:block,ep...)): (ep...))
                elseif contains(sh[en],"return")
                    js = join(split(sexpr[h],"return")[2:end],"return")
                    y = h
                    (h,state) = bematch(js,sexpr,h,iter,state)
                    $(if mode == :expr
                        quote
                            rp = $rfun(vcat(js,sexpr[y+1:h]...),be)
                            push!(nsr,Expr(:return,rp))
                        end
                    else
                        quote
                            rp = $rfun(vcat(js,sexpr[y+1:h]...),be) |> string
                            push!(nsr,"return "*rp)
                        end
                    end)
                elseif contains(sh[en],"for")
                    throw(ReduceError("for block parsing not supported"))
                elseif contains(sh[en],"end")
                    nothing
                elseif isempty(sh[en])
                    nothing
                elseif contains(sexpr[h], ":=")
                    sp = split(sexpr[h], ":=")
                    $(if mode == :expr
                        :(push!(nsr,Expr(:(=),parse(sp[1]),rparse(sp[2],be))))
                    else
                        :(push!(nsr, Compat.String(sp[1]) * ":=" * string($rfun(sp[2] |> Compat.String |> RExpr,be))))
                    end)
                elseif contains(sexpr[h],":")
                    sp = split(sexpr[h],":")
                    $(if mode == :expr
                        :(push!(nsr,Expr(:(:),rparse(sp[1],be),rparse(sp[2],be))))
                    else
                        :(push!(nsr,($rfun(sp[1],be)|>string)*":"*($rfun(sp[2],be)|>string)))
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
        $rfun(r::Array{Compat.String,1},be=0) = $fun(RExpr(r),be)
        $rfun(r,be=0) = $fun(r |> Compat.String |> RExpr, be)
    end
end
