\version "2.27.1"

\header {
  texidoc = "Textual variants of the common time and cut time symbols can be
printed with the @code{\\text-common-time} and @code{\\text-cut-time} markup
commands, which are both protected against overrides of @code{font-name}."
}

\markup \override #'(font-name . "Bitstream Vera Sans, Bold")
  \column {
    \line { common time: \tiny\text-common-time }
    \line { cut time: \tiny\text-cut-time }
  }
