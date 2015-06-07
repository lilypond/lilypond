\header {

  texidoc = "Identifiers following a chordmode section are not
interpreted as chordmode tokens.  In the following snippet, the
identifier `m' is not interpreted by the lexer as a minor chord
modifier."

}

\version "2.19.22"

myDisplayMusic =
#(define-void-function (music)
 (ly:music?)
 (display-scheme-music music (current-error-port)))

\myDisplayMusic \chordmode { c }

m = \relative { c'4 d e f }

\new Staff { \m }
