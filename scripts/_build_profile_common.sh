#!/usr/bin/env bash
set -euo pipefail

readonly SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
readonly REPO_ROOT="$(cd -- "${SCRIPT_DIR}/.." && pwd)"

usage() {
  local profile="$1"
  cat <<EOF
Usage: bash scripts/build-${profile}.sh [--deploy|--help]

Build profile: ${profile}
Options:
  --deploy  Deploy executables after a successful build
  --help    Show this help
EOF
}

parse_deploy_arg() {
  local profile="$1"
  shift

  RUN_DEPLOY=0
  case "${1-}" in
    "")
      ;;
    --deploy)
      RUN_DEPLOY=1
      ;;
    --help)
      usage "${profile}"
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage "${profile}" >&2
      exit 2
      ;;
  esac

  if [[ $# -gt 1 ]]; then
    echo "Too many arguments." >&2
    usage "${profile}" >&2
    exit 2
  fi
}

check_build_identity() {
  local branch suffix
  branch="$(git -C "${REPO_ROOT}" rev-parse --abbrev-ref HEAD)"
  suffix="$(
    sed -nE 's/^progVerSuff[[:space:]]*=[[:space:]]*"([^"]*)".*$/\1/p' \
      "${REPO_ROOT}/Main/Barbarossa.hs" | head -n 1
  )"

  if [[ -z "${suffix}" ]]; then
    echo "Could not read progVerSuff from Main/Barbarossa.hs." >&2
    exit 1
  fi

  if [[ "${suffix}" != "${branch}" ]]; then
    echo "Build identity mismatch: branch='${branch}' but progVerSuff='${suffix}'." >&2
    exit 1
  fi
}

maybe_deploy() {
  if [[ "${RUN_DEPLOY}" -eq 1 ]]; then
    (cd "${REPO_ROOT}" && python scripts/deploy_exes.py)
  fi
}
