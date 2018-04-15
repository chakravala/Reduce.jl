#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

using Documenter, Reduce

makedocs(
    # options
    modules = [Reduce],
    repo = "github.com/chakravala/Reduce.jl.git",
    doctest = false,
    format = :html,
    sitename = "Reduce.jl",
    authors = "Michael Reed",
    pages = Any[
        "Home" => "index.md"
        "Library" => "library.md"
        ]
)

deploydocs(
    target = "build",
    repo   = "github.com/chakravala/Reduce.jl.git",
    branch = "gh-pages",
    latest = "master",
    osname = "linux",
    julia  = "0.6",
    deps = nothing,
    make = nothing,
)
