\version "1.5.68"
\include "german-chords.ly"

% #(set! german-Bb #t)

ch = \chords { beses1/+beses bes/+bes b/+b bis/+bis ases/+ases as/+as a/+a ais/+ais fisis/+fisis}

\score {
   <
   \context ChordNames=chn {\ch}
   \context Staff=stf \chords {\ch}
   >
   \paper {}
}

