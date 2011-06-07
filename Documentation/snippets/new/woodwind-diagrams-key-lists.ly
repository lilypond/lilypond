\version "2.14.0"

\header {
  lsrtags = "winds"

  texidoc="
The snippet below produces a list of all possible keys and key
settings for woodwind diagrams as defined in
@file{scm/define-woodwind-diagrams.scm}.  The list will be displayed
on the console and in the log file, but not in the music.
"
  doctitle = "Woodwind diagrams key lists"
}

#(print-keys-verbose 'piccolo)
#(print-keys-verbose 'flute)
#(print-keys-verbose 'flute-b-extension)
#(print-keys-verbose 'oboe)
#(print-keys-verbose 'clarinet)
#(print-keys-verbose 'bass-clarinet)
#(print-keys-verbose 'low-bass-clarinet)
#(print-keys-verbose 'saxophone)
#(print-keys-verbose 'baritone-saxophone)
#(print-keys-verbose 'bassoon)
#(print-keys-verbose 'contrabassoon)
