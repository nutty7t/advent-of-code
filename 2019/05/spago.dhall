{ name = "advent-of-code-2019-day-05"
, dependencies =
    [ "console"
    , "control"
    , "debug"
    , "effect"
    , "node-fs"
    , "parsing"
    , "psci-support"
    , "spec"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
