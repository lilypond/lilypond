\header {

	texidoc =  "German chords use H/B iso. B/B-flat.

FIXME. Most likely broken during namespace reorganisation of early 1.7.

"


}
\version "1.7.6"
\include "german-chords-init.ly"

% #(set! german-Bb #t)

ch = \chords { beses1/+beses bes/+bes b/+b bis/+bis ases/+ases as/+as a/+a ais/+ais fisis/+fisis}

\score {
   <
   \context ChordNames=chn {\ch}
   \context Staff=stf \chords {\ch}
   >
   \paper {}
}

%% new-chords-done %%
