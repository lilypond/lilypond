\version "2.27.3"

\header {
  texidoc = "A fractional @code{beatStructure} does not disturb beamlet
placement.  Beat lengths are rational (not necessarily integral).  A
beamlet whose direction depends on the beat structure must still be
placed when the beat it points at is a fraction of @code{beatBase}."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  indent = 0
  ragged-right = ##t
  \context {
    \Score
    %% Timing_translator complains if timeSignatureSettings is empty.
    timeSignatureSettings = #'((dummy . dummy))
  }
}

%% The middle stem carries more beams than either neighbour, so beamify()
%% consults the beat structure to decide which way its beamlet points.

%% A structure given to \time, with the fractional beat first.
\score {
  \new Staff {
    \time #'(1/2 1 1) #'(5/2 . 4)
    c''16[ c'' c''8] c''16[ c'' c'' c''8]
  }
}

%% A structure derived from a fractional time signature places the
%% fractional beat last, so the beam has to come late enough in the measure
%% to reach it: (1 1 1/2) here, and (1 1 2/3) below.
\score {
  \new Staff {
    \time #'(5/2 . 4)
    c''4 c''4 c''16[ c'' c''8]
  }
}

\score {
  \new Staff {
    \time #'(8/3 . 4)
    c''4 c''4 c''16[ c'' c''8]
  }
}
