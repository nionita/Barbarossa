#!/usr/bin/env bash
set -euo pipefail

source "$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)/_build_profile_common.sh"

parse_deploy_arg "prod" "$@"
check_build_identity

(cd "${REPO_ROOT}" && stack build --flag Barbarossa:-qscollect Barbarossa:exe:Barbarossa)

# A release (empty progVerSuff, only on master): the engine goes to dist/Barbarossa-<version>[.exe]
if [[ -z "$(read_build_suffix)" ]]; then
  version="$(sed -nE 's/^Version:[[:space:]]*([^[:space:]]+).*$/\1/p' "${REPO_ROOT}/Barbarossa.cabal" | head -n 1)"
  bin_dir="$(cd "${REPO_ROOT}" && stack path --local-install-root)/bin"
  ext=""
  [[ -f "${bin_dir}/Barbarossa.exe" ]] && ext=".exe"
  mkdir -p "${REPO_ROOT}/dist"
  cp "${bin_dir}/Barbarossa${ext}" "${REPO_ROOT}/dist/Barbarossa-${version}${ext}"
  echo "Release engine: dist/Barbarossa-${version}${ext}"
fi

maybe_deploy
