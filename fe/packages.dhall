let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.9-20230718/packages.dhall
        sha256:661c257c997f37bba1b169020a87ae6ea08eb998e931875cb92e86ac9ea26846

in upstream
  with halogen-router =
      { dependencies =
          [ "aff"
          , "effect"
          , "either"
          , "foreign"
          , "halogen"
          , "halogen-hooks"
          , "halogen-store"
          , "halogen-subscriptions"
          , "maybe"
          , "prelude"
          , "routing"
          , "routing-duplex"
          , "safe-coerce"
          , "transformers"
          , "tuples"
          ]
      , repo =
          "https://github.com/katsujukou/purescript-halogen-router.git"
      , version =
          "v0.1.0"
      }
