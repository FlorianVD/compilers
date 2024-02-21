#!/usr/bin/env bash
set -Eeuo pipefail

usage()
{
    cat <<EOF >&2
Usage: $0 microcc-path input_file [MICROCC_OPTIONS...]

Wrapper around microcc that filters 'RUN' lines and FileCheck lines like 'CHECK'.
This way, the students do not get errors when they are running custom tests before
they have implemented comments.

Options:
-h, --help          Show this help.
EOF
}

positional=()
microcc_args=("")

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage; exit 0
            ;;
        -*)
            # Pass all unrecognised options to microcc.
            microcc_args+=("$1")
            shift
            ;;
        *)
            positional+=("$1")
            shift
            ;;
    esac
done
set -- "${positional[@]}"

if [[ $# -ne 2 ]]; then
    echo "Expected 2 positional arguments, but got $#."
    echo "Try '$0 --help' for more information."
    exit 1
fi

MICROCC_PATH=$1
INPUT_FILE=$2

$MICROCC_PATH $microcc_args <(awk '/^\/\/ RUN:/ || /^\/\/ CHECK/ { printf "%s", RT; next } { printf "%s%s", $0, RT }' $INPUT_FILE)
