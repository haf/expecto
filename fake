#!/usr/bin/env bash
source .env
set -eu
set -o pipefail

TOOL_PATH="$PWD/.fake"
FAKE="${TOOL_PATH}/fake"

if ! [ -e "$FAKE" ]; then
  echo "Installing FAKE"
  dotnet tool install fake-cli --tool-path "$TOOL_PATH"
fi

if ! [[ -x .paket/paket ]]; then
  echo "Installing Paket"
  PAKET_VERSION=$(cat paket.dependencies| head -n 5 | grep version | cut -d ' ' -f2)
  dotnet tool install paket --tool-path .paket --version $PAKET_VERSION
fi

"$FAKE" "$@"
