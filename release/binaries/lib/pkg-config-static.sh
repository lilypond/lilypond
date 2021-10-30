#!/bin/sh

static="--static"
if [ "$*" = "--version" ]; then
    # Old versions of pkgconf throw errors if querying --version with any other
    # argument, so avoid passing --static in that case.
    static=""
fi
exec pkg-config $static "$@"
