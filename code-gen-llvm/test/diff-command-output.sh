#!/usr/bin/env bash
set -Eeuo pipefail

usage()
{
    cat <<EOF >&2
Usage: $0 EXPECTED_PREFIX ACTUAL_PREFIX COMMAND [ARGS...] [OPTIONS]

Runs COMMAND with the given ARGS, and writes stdout and stderr to
ACTUAL_PREFIX.stdout and ACTUAL_PREFIX.stderr, respectively. Depending on the
exit code of the command, either stdout or stderr is compared to the
reference, stored in the files EXPECTED_PREFIX.stdout and
EXPECTED_PREFIX.stderr. If the command exits with error code 0, stdout is
compared; otherwise, stderr is compared. The script returns 0 if the actual
output matches the expected output, and 1 if it does not.

Options:
-h, --help          Show this help.
EOF
}

positional=()
while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage; exit 0
            ;;
        --)
            shift
            while [[ $# -gt 0 ]]; do
                positional+=("$1")
                shift
            done
            ;;
        -*)
            echo "Unknown command-line option '$1'."
            echo "Try '$0 --help' for more information."
            exit 1
            ;;
        *)
            positional+=("$1")
            shift
            ;;
    esac
done
set -- "${positional[@]}"

if [[ $# -lt 2 ]]; then
    echo "Expected at least 2 positional arguments, but got $#."
    echo "Try '$0 --help' for more information."
    exit 1
fi

EXPECTED_PREFIX="$1"
shift
ACTUAL_PREFIX="$1"
shift

$@ >$ACTUAL_PREFIX.stdout 2>$ACTUAL_PREFIX.stderr || true

if [[ -s "$EXPECTED_PREFIX.stderr" ]]; then
    diff $ACTUAL_PREFIX.stderr $EXPECTED_PREFIX.stderr
else
    diff $ACTUAL_PREFIX.stdout $EXPECTED_PREFIX.stdout
    diff $ACTUAL_PREFIX.stderr $EXPECTED_PREFIX.stderr
fi
