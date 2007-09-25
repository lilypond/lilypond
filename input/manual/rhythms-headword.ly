\version "2.11.33"

\layout {
   \context { \Score
      \override NonMusicalPaperColumn #'line-break-system-details =
      #'((alignment-offsets . (0 -14 -32 -46)))
   }
}

\new Score <<
   \set Score.proportionalNotationDuration = #(ly:make-moment 1 96)
   \set Score.autoBeaming = ##f
   \set Score.tupletFullLength = ##t
   \override Score.TupletBracket #'staff-padding = #5
   \new StaffGroup <<
      \new RhythmicStaff {
         \time 4/8
         c64 [
         c64
         c16
         c64
         c64
         c64
         c64
         c16
         c64
         c64 ]
         c64 [
         \set stemLeftBeamCount = #4
         \set stemRightBeamCount = #4
         c64
         \set stemLeftBeamCount = #4
         \set stemRightBeamCount = #3
         c64
         c32.
         c64
         c64
         c64
         c64
         c64
         \set stemRightBeamCount = #4
         c64
         \set stemLeftBeamCount = #4
         \set stemRightBeamCount = #3
         c64
         \set stemRightBeamCount = #2
         c32.
         \time 1/8
         \set stemLeftBeamCount = #2
         c64
         c64
         c16
         c64
         c64 ]
      }
      \new RhythmicStaff {
         \set stemLeftBeamCount = #0
         c16 [ ]
         r8.
         \times 2/3 {
            c16 [
            c16
            c16
            c16
            c16
            c16
         }
         \times 2/3 {
            c32
            c32
            c32
            c32
            c32 ]
            r32
         }
      }
   >>
   \new StaffGroup <<
      \new RhythmicStaff {
         \times 4/5 {
            \set stemLeftBeamCount = #0
            c32 [ ]
            r16.
            \set stemLeftBeamCount = #2
            c16. [
            c32
            \set stemRightBeamCount = #3
            c32 ] 
            r32
         }
         \times 8/11 {
            c16. [
            c8
            c8 ]
         }
         c8
      }
      \new RhythmicStaff {
         \times 2/3 {
            c16 [
            c8. ]
            c4
            r4
         }
         \times 4/5 {
            r16
            \set stemLeftBeamCount = #2
            \set stemRightBeamCount = #0
            c16. [ ]
         }
      }
   >>
>>
