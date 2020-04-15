\version "2.21.1"

\header {
  texidoc = "If @code{after-writing} is set in the @code{\\midi}
block, it is called after every MIDI file that is written.  The
visual and MIDI output are not important in this test."

  AA = "a0"
  EE = "e0"
}

#(ly:set-option 'warning-as-error #t)

\midi {
  after-writing =
  #(lambda (performance file-name)
    (let* ((headers (ly:performance-headers performance))
           (AA (ly:modules-lookup headers 'AA))
           (BB (ly:modules-lookup headers 'BB))
           (CC (ly:modules-lookup headers 'CC))
           (DD (ly:modules-lookup headers 'DD))
           (EE (ly:modules-lookup headers 'EE))
           (FF (ly:modules-lookup headers 'FF))
           (GG (ly:modules-lookup headers 'GG)))
     (ly:warning "~a ~a ~a ~a ~a ~a ~a" AA BB CC DD EE FF GG)))
}

\book {
  \header { BB = "b1" EE = "e1" FF = "f1" }
  \bookpart {
    \header { CC = "c2" FF = "f2" GG = "g2" }

    %% this score tests that variables can be introduced (A-D) and
    %% overridden (E-G)
    #(ly:expect-warning "a0 b1 c2 d3 e1 f2 g3")
    \score {
      { c'1 }
      \header { DD = "d3" GG = "g3" }
      \layout { }
      \midi { }
    }
  }
}
