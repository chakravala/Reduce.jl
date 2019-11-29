#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

import REPL.LineEdit
import ReplMaker

ans = nothing

"""
	finished(s)

Examine the buffer in the repl to see if the input is complete
"""
function finished(s)
    str = String(take!(copy(LineEdit.buffer(s))))
    if length(str) == 0
        return false
    elseif occursin(r"(^|\n)[^%\n]*(;|\$)[ ]*(%[^\n]*)?$",str)
        return true
    else
        return false
    end
end

"""
    parserepl(input)

Called on valid input from the repl.
"""
function parserepl(input) 
    if !isempty(strip(input))
        global ans = RExpr(input[1:end-1])
        if input[end] == ';'
            display(ans)
        end
    end
end

"""
    replinit(repl)

Start up the Reduce  repl mode.
"""
function repl_init(repl)
    ReplMaker.initrepl(
        parserepl,
        valid_input_checker=finished,
        prompt_text="reduce> ",
        prompt_color=:cyan,
        start_key='}',
        mode_name="REDUCE",
        repl=repl,
    )
end
