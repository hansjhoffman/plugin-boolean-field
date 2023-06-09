{ name = "my-project"
, dependencies =
  [ "bifunctors", "control", "either", "parsing", "prelude", "strings" ]
, license = "BSD-3-Clause"
, packages = ./packages.dhall
, repository = "https://github.com/hansjhoffman/plugin-boolean-field"
, sources = [ "src/**/*.purs" ]
}
