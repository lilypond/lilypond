\version "2.25.23"

\header {
  texidoc = "This test exercises the interpretation of textual tempo-change
instructions by @code{\\articulate}."
}

\include "articulate.ly"

\score {
  \articulate \fixed c' {
    %% major increase
    c4 d-"accel" e f-"a tempo" |
    c4 d-"accel." e f-"tempo I" |
    c4 d-"accelerando" e f-"a tempo" |
    %% minor increase
    c4 d-"poco accel." e f-"tempo I" |
    %% major decrease
    c4 d-"rall" e f-"a tempo" |
    c4 d-"Rall" e f-"tempo I" |
    c4 d-"rall." e f-"a tempo" |
    c4 d-"rit." e f-"tempo I" |
    %% minor decrease
    c4 d-"poco rall." e f-"a tempo" |
    c4 d-"poco rit." e f-"tempo I" |
  }

  \midi {}
}
