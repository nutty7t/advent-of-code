{ name =
    "my-project"
, dependencies =
    [ "console", "control", "effect", "node-fs", "psci-support", "spec" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
