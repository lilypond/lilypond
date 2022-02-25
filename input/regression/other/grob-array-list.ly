\version "2.23.4"

\header {
  texidoc = "Conversions are possible between grob arrays and
grob lists."
}

{
  \override NoteColumn.after-line-breaking =
    #(lambda (grob)
       (let* ((note-heads-array (ly:grob-object grob 'note-heads))
              (note-heads-list (ly:grob-array->list note-heads-array)))
         (if (not (equal? note-heads-list
                          (ly:grob-array->list
                            (ly:grob-list->grob-array
                              note-heads-list))))
             (ly:error "Test failed: conversion from list to array \
and back changes value."))))
  <c d e f g>
}

%% Test robustness

#(ly:set-option 'warning-as-error)

#(ly:expect-warning (G_ "ly:grob-list->grob-array expected a list"))
#(ly:grob-list->grob-array 42)

#(ly:expect-warning (G_ "ly:grob-list->grob-array encountered a non-grob object"))
#(ly:grob-list->grob-array '(42))
