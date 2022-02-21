\version "2.23.7"

\header {
  texidoc = "This shows the default format of @code{LyricRepeatCount}
and that it can be overridden."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  ragged-right = ##t
}

music = \fixed c' {
  \repeat volta 1 { f1 }
  \repeat volta 2 { g1 }
  \repeat volta 3 { a1 }
  \repeat volta 4 { b2 b }
}

words = \lyricmode {
  \repeat volta 1 { Once.1 }
  \repeat volta 2 { Twice.1 }
  \repeat volta 3 { Thrice.1 }
  \repeat volta 4 { Four2 times. }
}

\score {
  <<
    \new GregorianTranscriptionStaff <<
      \sectionLabel "GregorianTranscriptionLyrics"
      \music
    >>

    \new GregorianTranscriptionLyrics \with {
      stanza = "default:"
    } \words

    \new GregorianTranscriptionLyrics \with {
      stanza = "uppercase:"
      lyricRepeatCountFormatter = #(make-lyric-repeat-count-formatter
                                    '(roman-ij dot))
    } \words
  >>
}

\score {
  <<
    \new Staff <<
      \sectionLabel "Lyrics"
      \music
    >>

    \new Lyrics \with {
      stanza = "default:"
      \consists "Lyric_repeat_count_engraver"
    } \words

    \new Lyrics \with {
      stanza = "silly:"
      \consists "Lyric_repeat_count_engraver"
      lyricRepeatCountFormatter = #(make-lyric-repeat-count-formatter
                                    '(bold circle numbers))
      \override LyricRepeatCount.font-shape = #'()
    } \words
  >>
}
