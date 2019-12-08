{ name = "advent-of-code-2019-day-6"
, dependencies =
    [ "console"
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
