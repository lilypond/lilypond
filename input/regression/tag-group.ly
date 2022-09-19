\version "2.23.14"

\header {
  texidoc = "The operation of @code{\\keepWithTag} can be made more
flexible by using @code{\\tagGroup}."
}

\layout { ragged-right= ##t }

music =
<<
  \tag vI.vII
  \new Staff \with { instrumentName = "vI vII" }
  <<
    \tag vI \new Voice \with { \voiceOne }
    { g''-\tag slurs ( a'' g'' a''-\tag slurs ) }
    \tag vII \new Voice \with { \voiceTwo }
    { c''-\tag slurs ( d'' c'' d''-\tag slurs ) }
  >>
  \tag bI.bII
  \new Staff \with { instrumentName = "bI bII" \clef "bass" }
  <<
    \tag bI \new Voice \with { \voiceOne }
    { g-\tag slurs ( a g a-\tag slurs ) }
    \tag bII \new Voice \with { \voiceTwo }
    { c-\tag slurs ( d c d-\tag slurs ) }
  >>
>>

demo =
#(define-music-function (syms m)
  (symbol-list? ly:music?)
  #{
    \new Score <<
      \keepWithTag #syms #m
      \context Staff \textMark #(string-join (map symbol->string syms) "&")
    >>
  #})

#(set-global-staff-size 16)

demoline =
#(define-scheme-function (m1 m2) (ly:music? ly:music?)
  #{
    \markup \column { \fill-line { \null \score { #m1 } \score { #m2 } \null }
                      \vspace #1 }
  #})


\markuplist {
  \fill-line { \center-column \fontsize #5 \bold { "\\keepWithTag" } }
  \vspace #1
}

\demoline
\demo #'(vI vII bI bII slurs) \music
\demo #'(slurs vI) \music
\demoline
\demo #'(vI bI bII) \music
\demo #'(vI bI bII none) \music

\markup \vspace #2

\markuplist {
  \fill-line { \center-align \fontsize #5 \bold \left-column
	       { "\\tagGroup vI.vII" "\\tagGroup bI.bII" } }
  \vspace #1
}

\tagGroup vI.vII
\tagGroup bI.bII

\demoline
\demo #'(vI vII bI bII slurs) \music
\demo #'(slurs vI) \music
\demoline
\demo #'(vI bI bII) \music
\demo #'(vI bI bII none) \music
