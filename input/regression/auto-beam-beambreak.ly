\version "2.27.0"

\header {
  texidoc = "Autobeaming can be adjusted using @code{\\beamBreak} and
@code{\\noBeamBreak}."
}

#(define (invert-predicate pred?)
   (lambda (arg . rest)
     (not (apply pred? (cons arg rest)))))

#(define (remove-typed-music mus types)
   (music-filter (invert-predicate (music-type-predicate types))
                 (ly:music-deep-copy mus)))

#(define (remove-beam-control mus)
   (remove-typed-music mus '(beam-break-event beam-event beam-forbid-event)))

"\\|" = \beamBreak
"\\_" = \noBeamBreak

\parallelMusic beamBreaks,manualBeams {
  %% split beam groups
  8  8 \| 8  8  8  8 8\| 8  |
  8[ 8]   8[ 8] 8[ 8 8]  8] |

  %% connect two beam groups
  8 8 8 8 \_ 8 8 8 8   |
  8[ 8 8 8   8 8 8 8 ] |

  %% manual beaming by [ ] and \noBeam always takes precedence
  8 8[ 8 8\| 8 8] 8 \_ 8\noBeam |
  8 8[ 8 8   8 8] 8 8\noBeam    |

  %% split off individual notes
  8 \|     8 8 8 8 \| 8 \|      8 8 |
  8\noBeam 8 8 8 8    8 \noBeam 8 8 |

  %% connect beams over breathing signs
  8  8 \_ \breathe 8 8  8  8 \breathe \_ 8 8  |
  8[ 8 \breathe    8 8] 8[ 8 \breathe    8 8] |

  %% connect beams over bar lines
  8 8 8 8 8  8 8 8 \_ |
  8 8 8 8 8[ 8 8 8    |
  %
  8 8 8 8  8 8 8 8    |
  8 8 8 8] 8 8 8 8    |

  %% \noBeamBreak also creates new beams
  \time 3/8 4 8\_ 8 4 |
  \time 3/8 4 8[ 8] 4 |
}

#(assert
  (equal? ((@@ (lily) music->lily-string)
           (remove-beam-control beamBreaks))
          ((@@ (lily) music->lily-string)
           (remove-beam-control manualBeams)))
  "\\beamBreaks and \\manualBeams should agree in substance.")

\new RhythmicStaff
<<
  {
    <>-"Effect of \\beamBreak and \\noBeamBreak"
    \beamBreaks
  }
  \\
  {
    <>-"Desired result"
    \manualBeams
  }
>>
