\version "2.16.0"

\header {
  texidoc = "The property @code{chordNameExceptions} can be used
to store a list of special notations for specific chords."
}

% Denote '7sus4' with '^7 wahh'.
chExceptionMusic = { <c f g bes>-\markup { \super "7" "wahh" } }

% Add it to existing exceptions.
chExceptions =
#(append (sequential-music-to-chord-exceptions chExceptionMusic #t)
         ignatzekExceptions)

theMusic = \chordmode {
  \time 2/4
  c:7sus4 c:dim7/+f \break
  \set chordNameExceptions = #chExceptions
  c:7sus4 c:dim7/+f
}

\layout {
  ragged-right = ##t
}

<<
  \context ChordNames \theMusic
  \context Voice \theMusic
>>
