#!/bin/sh
#
# Indent and untabify source files (given by their
# filenames in the command line), according to
# LilyPond source style standards.

# This is needed in order to load our .dir-locals.el, which contains
# configuration relevant for Scheme indentation.
load_dir_locals='
     (setq enable-local-variables :all)'

reformat_file='(progn
     (delete-trailing-whitespace)
     (indent-region (point-min) (point-max) nil)
     (untabify (point-min) (point-max))
     (save-buffer))'
for f in "$@"; do
  # First --eval to load .dir-locals.el before file is loaded, so
  # the options take effect, then second --eval after file is loaded,
  # to process it.
  emacs --no-init-file --no-site-file \
        --batch --eval "${load_dir_locals}" "$f" --eval "${reformat_file}"
done
