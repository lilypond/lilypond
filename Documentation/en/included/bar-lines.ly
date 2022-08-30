\version "2.23.13"

#(use-modules (ice-9 match))

\layout {
  \context {
    \Staff
    \remove Clef_engraver
    \remove Time_signature_engraver
  }
  \context {
    \PianoStaff
    \unset systemStartDelimiter
  }
  \context {
    \Score
    \override SectionLabel.font-size = 0
  }
  line-width = 4.5\cm
  ragged-right = ##f
  system-count = 1
}

group-glyphs-alist =
  #'(("Simple bar lines" . ("|" "|-s" ""
                             "!" ";"))
     ("End-of-line bar lines" . ("x-|" "x-||" "x-."))
     ;; "|" and "||" are restated here on purpose
     ("Chant bar lines" . ("'" "," "|"
                            "||" "-span|" "k"))
     ("Section bar lines" . (".|" "|.|" "|."
                              ".|-|" "." ".."
                              ".|-||" "||"))
     ("Segno bar lines" . ("S" "S-||" "|.S"
                            "S-|" "S-S" "|.S-S"))
     ("Start-repeat bar lines" . ("S.|:" ".|:" "[|:"
                                   "S.|:-|" ".|:-|" "[|:-|"
                                   "S.|:-||" ".|:-||" "[|:-||"
                                   "|.S.|:" ".|:-|." "[|:-|."
                                   "|.S.|:-S" "S.|:-S"))
     ("Double-repeat bar lines" . (":..:" ":.|.:" ":|.S.|:"
                                    ":|.:" ":|.|:" ":|.S.|:-S"
                                    ":|][|:"))
     ("End-repeat bar lines" . (":|." ":|.S" #f
                                 ":|]" ":|.S-S")))

seen = #(make-hash-table)

\markuplist
\override #'(padding . 4)
#(map
  (match-lambda
   ((group-title . glyphs)
    #{
      \markup \column {
        \large #group-title
        \vspace #0.3
        \override #'(baseline-skip . 20)
        \table #(make-list 3 LEFT)
          #(map
            (lambda (glyph)
              (if (not glyph) ; allow #f for alignment purposes
                  #{ \markup \null #}
                  (begin
                   (hash-set! seen glyph #t)
                    #{
                      \markup \score {
                        \new PianoStaff <<
                          \new Staff {
                            \sectionLabel \markup \typewriter #(string-append "\"" glyph "\"")
                            \clef treble
                            \key f \major
                            \bar #glyph
                            bes'1
                            \bar #glyph
                            bes'1
                            \bar #glyph
                          }
                          \new Staff {
                            \clef bass
                            \key f \major
                            d1
                            d1
                          }
                        >>
                      }
                    #})))
            (reverse glyphs))
      }
    #}))
  group-glyphs-alist)

% Check that all bar types are included in the alist above.
#(for-each
  (match-lambda
   ((glyph . _)
    (when (not (hash-ref seen glyph))
      (ly:error "Bar type ~s must be added to Documentation/en/included/bar-lines.ly"
                glyph))))
  bar-glyph-alist)
