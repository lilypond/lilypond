\version "1.3.146"
\header { texidoc = "
Conventionally, stems and beams extend to the middle staff line.  This
extension can be controlled through @code{Voice.Stem}'s grob-property
@code{no-stem-extend}:
"}

\paper { linewidth = -1.}
\score {\notes \relative c {
  \grace a'8 a4
  \property Voice.Stem \set #'no-stem-extend = ##t
  \grace g8 g4 [g8 g]
}}
