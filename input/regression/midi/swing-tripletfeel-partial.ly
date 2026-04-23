\version "2.27.1"

\header {
  texidoc = "The @code{\\tripletFeel} function automatically detects
@code{\\partial} (anacrusis) and calculates the correct offset.
@code{find-leading-partial} must return the @code{PartialSet} node for each partial
variant, and @code{\\tripletFeel} must compile without error for all of them,
including simultaneous music with @code{\\partial} in multiple voices."
}

#(ly:set-option 'warning-as-error #t)

\include "swing.ly"

musicPartialQuarter = { \time 4/4 \partial 4   c'8 c'8 | }
musicPartialEighth  = { \time 4/4 \partial 8   c'8 | c'8 }
musicPartialDotted  = { \time 4/4 \partial 4.  c'8 c'8 c'8 | }
musicPartialHalf    = { \time 4/4 \partial 2   c'8 c'8 c'8 c'8 | }
musicNoPartial      = { \time 4/4              c'8 c'8 c'4 c'2 | }
%% Two voices each with \partial 8 -- \tripletFeel should
%% adjust all partials, otherwise there is a "conflict with event:
%% partial-event" warning because the two voices' partial durations diverge.
musicSimultaneousPartial = {
  \time 4/4
  <<
    \new Voice { \partial 8 d'8 | d'8 d'8 d'4 d'2 }
    \new Voice { \partial 8 b8  | b8  b8  b4  b2  }
  >>
}

%% Assert that find-leading-partial returns the correct duration moment for each
%% partial.  Without the patch, the sequential-music search stops at the
%% first element, so \time 4/4 \partial ... returns #f.
#(for-each
  (lambda (music expected)
    (let* ((partial (find-leading-partial music))
           (got     (if partial
                        (ly:moment-main
                         (ly:duration->moment
                          (ly:music-property partial 'duration)))
                        0)))
      (if (not (= got expected))
          (ly:error "find-leading-partial: expected ~a, got ~a" expected got))))
  (list musicPartialQuarter musicPartialEighth
        musicPartialDotted  musicPartialHalf
        musicNoPartial)
  (list 1/4 1/8 3/8 1/2 0))

% Have multiple scores here to test every partial constellation for itself
\score {
  \new Staff {
    \tripletFeel 8 \musicPartialQuarter
  }
  \midi { }
}

\score {
  \new Staff {
    \tripletFeel 8 \musicPartialEighth
  }
  \midi { }
}

\score {
  \new Staff {
    \tripletFeel 8 \musicPartialDotted
  }
  \midi { }
}

\score {
  \new Staff {
    \tripletFeel 8 \musicPartialHalf
  }
  \midi { }
}

\score {
  \new Staff {
    \tripletFeel 8 \musicNoPartial
  }
  \midi { }
}

\score {
  \new Staff {
    \tripletFeel 8 \musicSimultaneousPartial
  }
  \midi { }
}
