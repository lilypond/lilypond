\header
{

  texidoc = "test various markup commands."

}
\paper { ragged-right = ##T }
\version "2.21.0"

\score{
  {
    g'_\markup {
      \column {
        \line {
          foo \magnify #2 foo
          LOWER \lower #3 LOWER
          \large \bold { normal \normal-text normal }
          Small-Caps \smallCaps 	Small-Caps
        }

        \override #'((line-width . 50) (bla . "This is a field containing text. Blah blah
  blah.  This is a field containing text. Blah blah blah.  This is a
  field containing text. Blah blah blah.  This is a field containing
  text. Blah blah blah. This is a field containing text. Blah blah
  blah."))
        \column  {
          justify:
          \justify-field #'bla
          wordwrap:
          \wordwrap-field #'bla
        }

        draw-line: \draw-line #'(5 . 3)
        \underline "underlined"
        \underline \underline \underline "multiple underlines"
      }
    }
  }

  \layout {
    \context {
      \Score
      \override PaperColumn.keep-inside-line = ##f
    }
  }
}
