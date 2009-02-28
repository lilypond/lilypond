\version "2.12.0"
\header {
  lsrtags = "pitches,world-music"
  texidoc = "Makam is a type of melody from Turkey using 1/9th-tone
microtonal alterations.  Consult the initialization file
@code{makam.ly} (see the `Learning Manual @version{},
4.6.3 Other sources of information' for the location of this file)
for details of pitch names and alterations."
  doctitle = "Makam example"
}

% Initialize makam settings
\include "makam.ly"

\relative c' {
  \set Staff.keySignature = #`((6 . ,(- KOMA)) (3 . ,BAKIYE))
  c4 cc db fk
  gbm4 gfc gfb efk
  fk4 db cc c
}
