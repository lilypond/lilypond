\version "1.3.146"
\header {
texidoc = "

There are several ways to calculate the direction of a beam.
@table @code
@item majority
number count of up or down notes
@item mean
mean center distance of all notes
@item median
mean centre distance weighted per note
@end table

You can spot the differences of these settings from these simple
examples:

These beam direction functions are defined in @file{scm/beam.scm}.  If
your favourite algorithm isn't one of these, you can hook up your own.
"
}

\paper { linewidth = -1.}
\score { \notes \relative c { 
  [d''8 a]
  \property Voice.Beam \set #'dir-function = #beam-dir-mean
  [d a] 
  \property Voice.Beam \set #'dir-function = #beam-dir-median
  [d a]
}}
\score { \notes \relative c {
  \time 3/8
  [d''8 a a]
  \property Voice.Beam \set #'dir-function = #beam-dir-mean
  [d a a] 
  \property Voice.Beam \set #'dir-function = #beam-dir-median
  [d a a] 
}}

