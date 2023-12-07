{ name = "halogen-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "argonaut"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "halogen"
  , "halogen-bootstrap5"
  , "halogen-hooks"
  , "halogen-router"
  , "http-methods"
  , "maybe"
  , "prelude"
  , "routing-duplex"
  , "tuples"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs"
  , "${if    (env:PRODUCTION : Bool) ? False
       then  "config/config.prd.purs"
       else  "config/config.dev.purs"}"
  , "test/**/*.purs"
  ]
}
