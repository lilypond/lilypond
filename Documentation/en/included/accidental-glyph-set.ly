\version "2.23.3"

\paper {
  indent = 4
  score-markup-spacing.basic-distance = 7
}

#(for-each
    (lambda (alist-name)
      (let ((name-string (symbol->string alist-name))
            (alist (eval alist-name (interaction-environment))))
        (add-score
          #{
            \score {
              \header {
                piece = \markup \typewriter #name-string
              } {
                \cadenzaOn
                \accidentalStyle dodecaphonic
                \set Staff.alterationGlyphs = #alist
                #(make-sequential-music
                   (map
                     (lambda (alteration)
                       (make-music 'NoteEvent
                                   'pitch
                                   (ly:make-pitch 1 0 alteration)
                                   'duration #{ 4 #}))
                     (sort
                       (map car alist)
                       <)))
              }
              \layout { }
            }
          #})))
    '(standard-alteration-glyph-name-alist
      alteration-hufnagel-glyph-name-alist
      alteration-medicaea-glyph-name-alist
      alteration-vaticana-glyph-name-alist
      alteration-mensural-glyph-name-alist
      alteration-kievan-glyph-name-alist))
