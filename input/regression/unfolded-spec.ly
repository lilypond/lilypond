\version "2.23.0"

\header {
  texidoc="@code{\\unfolded} hides music until a repeat is unfolded.
In this case, a second staff appears when the piece is unfolded."
}

music = \context Staff = A \fixed c' \repeat volta 2 <<
  s1
  \unfolded \context Staff = B s1
>>

\score { \music }
\score { \unfoldRepeats \music }
