\version "2.27.5"

\header {
  texidoc = "
A grace note directly before a @code{\\repeat volta} must not suppress
the start repeat bar, even with a single voice."
}

\layout { ragged-right = ##t }

\new Voice { \grace f'8 \repeat volta 2 b'1 }
