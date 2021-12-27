\version "2.23.0"

\header {
  texidoc="@code{\\volta} is useful for nth-time-only music.  Desired
explanatory text must be added manually."
}

music = \context Voice \fixed c' \repeat volta 2 {
  f1
  %% One can achieve something similar using \alternative and
  %% overriding the bracket label.  Besides looking a bit different,
  %% \alternative potentially uses alternative bar numbers.
  <<
    \volta #'() {
      \once \override TextSpanner.bound-details.left.text = "2nd time tacet"
      s1*2\startTextSpan s1\stopTextSpan
    }
    \volta 1 { g1~ 1~ 1 }
    \volta 2 \unfolded R1*3
  >>
  a1
}

\score { \music }
\score { \unfoldRepeats \music }
