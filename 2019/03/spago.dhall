{ name = "my-project"
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
