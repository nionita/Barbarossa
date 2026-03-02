#!/usr/bin/env bash
set -euo pipefail

source "$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)/_build_profile_common.sh"

parse_deploy_arg "prod" "$@"
check_build_identity

(cd "${REPO_ROOT}" && stack build --flag Barbarossa:-qscollect Barbarossa:exe:Barbarossa)
maybe_deploy
