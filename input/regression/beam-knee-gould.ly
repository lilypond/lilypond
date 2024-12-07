\version "2.25.23"

\header {
  texidoc = "Examples of kneed beams of varying complexity (based on how stem
  direction changes within a subdivision) all copied from  pgs.@tie{}316-317 of
  Gould's @emph{Behind Bars}."
}

\layout {
  indent = #0
  ragged-right = ##t
}

{  
  \stopStaff
  \omit Staff.Clef
  \omit Staff.TimeSignature
  \override Beam.auto-knee-gap = 0

  \set subdivideBeams = ##t

  c8 [c''16 16] 
  \once \set subdivideBeams = ##f
  c16 [c c c''8]
  c''8 [c16 8 c''16] s | \break
  c''32 [32 32 32 c16. 32 c''16 16] s8
  \once \set subdivideBeams = ##f
  c''32 [32 32 32 c16. 32 c''16 16] s8
  | \break
  c''16[c c c'' c c'' c] s
  \once \set subdivideBeams = ##f
  c''16[c c c'' c c'' c] s
  |
  \once \set beamMinimumSubdivision = #1/4
  c''16[c c c'' c c'' c] s
  c''16[c c c'' c \set stemRightBeamCount = 2 c'' c] s
  | \break

  % opposite stem direction
  c''16 [c c8] c16 [c'' c8] c16 [c'' c''8] s4 | \break
  c''16 [c'' c c''8] s8.
  \once \set subdivideBeams = ##f
  c''16 [c'' c c''8] s8.
  | \break

  % at end
  c''8 [16 c] c''8 [c16 c''] c8 [c''16 c] c8 [16 c''] | \break

  % at end w/ opposite stem direction
  c''16 [c'' c c'' c'' c] s8 s2 | \break
  c''8 [c16 c'' c'' c] s8
  \once \set subdivideBeams = ##f
  c''8 [c16 c'' c'' c] s8
  | \break
  c32 [c'' c'' c'' c c'' c'' c'']
  \once \set subdivideBeams = ##f
  c32 [c'' c'' c'' c c'' c'' c'']
  \once \set beamMinimumSubdivision = #1/8
  c32 [c'' c'' c'' c c'' c'' c'']
  \once \set beamMaximumSubdivision = #1/16
  c32 [c'' c'' c'' c c'' c'' c'']
  | \break

}
