\version "2.27.0"

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

    \new GregorianTranscriptionLyrics {
      \stanza "default:"
      \words
    }

    \new GregorianTranscriptionLyrics \with {
      lyricRepeatCountFormatter = #(make-lyric-repeat-count-formatter
                                    '(roman-ij dot))
    } {
      \stanza "uppercase:"
      \words
    }
  >>
}

\score {
  <<
    \new Staff <<
      \sectionLabel "Lyrics"
      \music
    >>

    \new Lyrics \with {
      \consists "Lyric_repeat_count_engraver"
    } {
      \stanza "default:"
      \words
    }

    \new Lyrics \with {
      \consists "Lyric_repeat_count_engraver"
      lyricRepeatCountFormatter = #(make-lyric-repeat-count-formatter
                                    '(bold circle numbers))
      \override LyricRepeatCount.font-shape = #'()
    } {
      \stanza "silly:"
      \words
    }
  >>
}
