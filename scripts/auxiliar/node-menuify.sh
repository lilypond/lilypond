#!/bin/sh

# Use Emacs to update @menu sections in Texinfo files given on the command line.

for f in "$@"; do
  emacs --no-init-file --no-site-file \
        --batch "$f" --eval "(progn (texinfo-all-menus-update) (save-buffer))"
done
