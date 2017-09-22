#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

using Documenter, Reduce

makedocs(
    # options
    modules = [Reduce],
    deps   = Deps.pip("mkdocs", "python-markdown-math"),
    repo = "github.com/chakravala/Reduce.jl.git",
    julia  = "0.4",
    osname = "osx",
    doctest = false
)

