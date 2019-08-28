#!/bin/sh

# By the default settings,
# XeTeX (xdvipdfmx) replaces link destination names with integers.
# In this case, the replaced destination names of
# remote PDF cannot be known. In order to avoid replacement,
# we can use commandline option `-C 0x0010' for xdvipdfmx.

exec xetex -8bit --output-driver='xdvipdfmx -C 0x0010' "$@"
