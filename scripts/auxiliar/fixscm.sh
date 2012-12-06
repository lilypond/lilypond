#!/bin/sh
#
# Indent and untabify source files (given by their
# filenames in the command line), according to
# LilyPond source style standards.

elisp_expression='(progn
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  (save-buffer))'
for f in "$@"; do
  emacs -batch "$f" --eval "${elisp_expression}"
done
