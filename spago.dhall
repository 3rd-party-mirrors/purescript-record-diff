{ name = "record-diff"
, dependencies =
  [ "console"
  , "effect"
  , "expect-inferred"
  , "prelude"
  , "psci-support"
  , "record"
  , "typelevel-prelude"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
