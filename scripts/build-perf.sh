#!/usr/bin/env bash
set -euo pipefail

source "$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)/_build_profile_common.sh"

parse_deploy_arg "perf" "$@"
check_build_identity

(cd "${REPO_ROOT}" && stack build --flag Barbarossa:-qscollect --flag Barbarossa:reproselfplay Barbarossa:exe:SelfPlay)
maybe_deploy
