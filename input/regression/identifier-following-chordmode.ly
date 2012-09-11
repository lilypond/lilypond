\header {

  texidoc = "Identifiers following a chordmode section are not
interpreted as chordmode tokens.  In the following snippet, the
identifier `m' is not interpreted by the lexer as a minor chord
modifier."

}

\version "2.16.0"

myDisplayMusic =
#(define-music-function (parser location music)
 (ly:music?)
 (display-scheme-music music (current-error-port))
 (make-music 'SequentialMusic 'void #t))

\myDisplayMusic \chordmode { c }

m = \relative c' { c4 d e f }

\new Staff { \m }
