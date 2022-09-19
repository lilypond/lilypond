\version "2.23.14"
\header {

  texidoc = "The @code{\\removeWithTag} and @code{\\keepWithTag}
commands can name multiple tags to remove or to keep."

}

\layout { ragged-right= ##t }

music =
<<
  \tag #'flood \new Voice { \voiceOne \repeat unfold 16 { c'''8 cis'''8 } }
  \tag #'highball \new Voice { \voiceThree \repeat unfold 8 { a'4( as') } }
  \tag #'buffoon \new Voice { \voiceFour \repeat unfold 2 { f1~ 1 } }
>>

demo =
#(define-music-function (fun syms m)
  (ly:music-function? symbol-list? ly:music?)
  #{
    \new Staff
    <<
      { \textMark #(string-join (map symbol->string syms) "&") \skip 1*4 }
      $fun #syms #m
    >>
  #})

#(set-global-staff-size 16)

\markuplist {
  \fill-line { \center-column \fontsize #5 \bold { "\\keepWithTag" } }
  \vspace #1
}
\demo #keepWithTag #'(none) \music
\demo #keepWithTag #'(flood highball buffoon) \music
\demo #keepWithTag #'(flood buffoon) \music
\demo #keepWithTag #'(buffoon) \music

\markup \vspace #2

\markuplist {
  \fill-line { \center-column \fontsize #5 \bold { "\\removeWithTag" } }
  \vspace #1
}
\demo #removeWithTag #'(none) \music
\demo #removeWithTag #'(flood highball buffoon) \music
\demo #removeWithTag #'(flood buffoon) \music
\demo #removeWithTag #'(buffoon) \music
