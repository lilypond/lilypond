\version "1.7.16"
\header { texidoc = "
Conventionally, stems and beams extend to the middle staff line.  This
extension can be controlled through @code{Voice.Stem}'s grob-property
@code{no-stem-extend}:
"}

\paper { raggedright = ##t}
\score {\notes \relative c \context Voice {
  \grace a'8 a4
  \property Voice.Stem \set #'no-stem-extend = ##t
  \grace g8 g4  g8-[ g]
}}
%% new-chords-done %%
