{ name =
    "advent-of-code-2019-day-01"
, dependencies =
    [ "aff", "console", "effect", "node-fs", "psci-support", "spec", "strings" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
