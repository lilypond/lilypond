\version "2.11.61"

\header
{
  title  = "Proportional notation"
  copyright = "© 2005 Trevor Bača - verbatim reproduction permitted."
}

\layout
{
 indent = #0.0
 \context {
   \Voice
   \remove "Forbid_line_break_engraver"
  \override  TupletNumber #'text = #tuplet-number::calc-fraction-text
   tupletFullLength = ##t
   \override Beam #'breakable = ##t
 }
 \context {
   \Score
   \override TupletBracket #'edge-text = #(cons
					   (markup #:arrow-head X LEFT #f)
					   (markup #:arrow-head X RIGHT #f))
   \override SpacingSpanner #'uniform-stretching = ##t
   \override SpacingSpanner #'strict-note-spacing = ##t
   proportionalNotationDuration = #(ly:make-moment 1 64)
   \override TimeSignature #'break-visibility = #end-of-line-invisible
   \override Beam #'break-overshoot = #'(-0.5 . 1.0)
   \override TupletBracket #'break-overshoot = #'(-0.5 . 1.0)
   \override TupletBracket #'staff-padding = #3.5
 }
 \context {
   % we want over print if necessary.
   \RhythmicStaff
   \remove "Separating_line_group_engraver"
 }
}

staffKind = "RhythmicStaff"

%staffKind = "Staff"

\relative c''
\new StaffGroup <<
  \new \staffKind <<
    {
      \skip 2
      \skip 2
      \break \time 4/8
      \skip 1 \break \time 4/8
      \skip 1 \break \time 4/8
    }

    {
      \time 4/8

      \times 7/9 {
	\times 4/6 {
	  r8 c32[ c c c c c c c] r4
	  c32[ c32 c16 }
	  \times 5/4 {
	    c16 c c] c32[ c32 c16 c] r8 }
      }

      \times 10/12 {
	\times 7/6 {
	  c32[ c32 c8 c16] r4
	  c16[ c16 c16. c32
	     }
	  \times 5/8 {
	    c16 c16 c16. c32] r8 c8[ c8] r4.
	}
      }

      \times 4/7 {
	r8
	c16[ c16
	     \times 5/4 {
	       c16 r16 c8 c c
	     }
	   }

	\times 3/4 {
	  c8]
	c16[ c
	     \times  2/3 {
	       c16 c16]
	     r4 }
      }
    }
  >>
  \new \staffKind
  <<
    {
      \times 9/5 {
	r8. c16[ c c
		 \grace {
		   \stemDown
		   c32[ c32]
		   \stemNeutral
		 }
		 c16 c c
		 c
	       }
	\times 4/7 {
	  c16 c c c ]
	\times 5/4 {
	  c16[ c32 c32]
	  r4
	  c32[ c c16
	     }
	}
	\times 10/12 {
	  \times 7/10 {
	    c16 c16 c8] r4 c4
	  c16[ c8 c16 c16 r8 c16
	     }
	  c16 c32 c32]
	r4.
	c16.[
	  c32
	}
	c16 c16]

    }
  >>
>>
