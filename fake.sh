#!/usr/bin/env bash
source .env
set -eu
set -o pipefail

TOOL_PATH="$PWD/.fake"
FAKE="${TOOL_PATH}/fake"

if ! [ -e "$FAKE" ]; then
  dotnet tool install fake-cli --tool-path "$TOOL_PATH"
fi
"$FAKE" "$@"
