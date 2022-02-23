\version "2.23.7"

% This file is used to compile LilyPond's Scheme files into bytecode. It does
% not produce any output, it just needs to load all (relevant) modules.
#(use-modules
  (lily accreg)
  ; skipped: documentation-*.scm, document-*.scm
  (lily framework-cairo)
  (lily framework-ps)
  (lily framework-svg)
  (lily graphviz)
  ; skipped: hyphenate-internal-words.scm
  ; skipped: guile-debugger because it doesn't work with Guile 2
  ; (lily guile-debugger)
  ; skipped: lily-sort.scm
  (lily output-ps)
  (lily output-svg)
  (lily page)
  (lily song)
  (lily song-util)
  (lily to-xml)
)
