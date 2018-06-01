#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

using Documenter, Reduce

makedocs(
    # options
    modules = [Reduce],
    doctest = false,
    format = :html,
    sitename = "Reduce.jl",
    authors = "Michael Reed",
    pages = Any[
        "Home" => "index.md",
        "Library" => "library.md",
        "User's Manual" => Any[
            "man/acknowledgement.md",
            "man/01-introduction.md",
            "man/02-structure.md",
            "man/03-expressions.md",
            "man/04-lists.md",
            "man/05-statements.md",
            "man/06-commands-declarations.md",
            "man/07-prefix-ops.md",
            "man/08-display.md",
            "man/09-polynomials.md",
            "man/10-properties.md",
            "man/11-substitution.md",
            "man/12-file-io.md",
            "man/13-interactive.md",
            "man/14-matrix.md",
            "man/15-procedures.md",
            "man/16-packages.md",
            "man/17-symbolic.md",
            "man/18-physics.md",
            "man/19-rlisp.md",
            "man/20-maintaining.md",
            "man/A-reserved.md",
            "man/B-bibliography.md",
            "man/C-changelog.md",
            "man/index.md"
            ]
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
