#!/usr/bin/env bash
set -e

npx elm-codegen install

# Runs elm make to make sure the dependencies have been downloaded to ELM_HOME
# and can be read by elm-codegen.
(cd review/ && elm make src/ReviewConfig.elm)

ELM_HOME="${ELM_HOME:-$HOME/.elm}"
echo $ELM_HOME

codegen() {
  # $1 : package name
  # $2 : package version
  docsJsonFile="$ELM_HOME/0.19.1/packages/elm/$1/$2/docs.json"
  elm-codegen run codegen/Generate.elm --flags-from=$docsJsonFile --output=src
}

codegen "core" "1.0.5"
codegen "html" "1.0.0"
codegen "json" "1.1.3"
codegen "parser" "1.1.0"
codegen "random" "1.0.0"

elm-format --yes src/Fn/
