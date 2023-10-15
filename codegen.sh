#!/usr/bin/env bash
set -e

npx elm-codegen install

# Runs elm make to make sure the dependencies have been downloaded to ELM_HOME
# and can be read by elm-codegen.
(cd review/ && elm make src/ReviewConfig.elm)

ELM_HOME="${ELM_HOME:-$HOME/.elm}"
echo $ELM_HOME

codegen() {
  # $1 : package author
  # $2 : package name
  # $3 : package version
  docsJsonFile="$ELM_HOME/0.19.1/packages/$1/$2/$3/docs.json"
  elm-codegen run codegen/Generate.elm --flags-from=$docsJsonFile --output=src
}

codegen "elm" "core" "1.0.5"
codegen "elm" "html" "1.0.0"
codegen "elm" "json" "1.1.3"
codegen "elm" "parser" "1.1.0"
codegen "elm" "random" "1.0.0"
codegen "elm-explorations" "test" "2.1.1"

elm-format --yes src/Fn/
