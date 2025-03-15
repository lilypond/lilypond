\version "2.25.25"

theHeader = \markuplist {
  ""
  "(def.)" "" ""
  "fs -5" "" ""
  "fs 0" "" ""
}

theRow = \markuplist {
  \on-the-fly #(lambda (layout props m)
                (interpret-markup layout props
                 (make-normal-text-markup
                  (symbol->string
                   (chain-assoc-get 'nested-fraction-orientation props
                    (string->symbol "(default)"))))))
  ""
  {
    \compound-meter #'(5/2 4)
    \compound-meter #'(2 8/3)
    \compound-meter #'(1/2 3/4)
  }
  \override #'(nested-fraction-relative-font-size . -5)
  {
    \compound-meter #'(5/2 4)
    \compound-meter #'(2 8/3)
    \compound-meter #'(1/2 3/4)
  }
  \override #'(nested-fraction-relative-font-size . 0)
  {
    \compound-meter #'(5/2 4)
    \compound-meter #'(2 8/3)
    \compound-meter #'(1/2 3/4)
  }
}

\markuplist {
  \test-override
  \override #'(baseline-skip . 5)
  \override #'(padding . 3)
  \table #'(-1 -1 -1 -1 -1 -1 -1 -1 -1 -1) {
    \theHeader
    \theRow
    \override #'(nested-fraction-orientation . vertical)
    \theRow
    \override #'(nested-fraction-orientation . horizontal)
    \theRow
  }
}
