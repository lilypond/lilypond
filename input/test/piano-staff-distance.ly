
\header
{

 texidoc = "It is possible to have different staff distances between
the staffs of a piano system, but it requires some advanced Scheme code. 
Currently, this is for testing purposes.  "

}

\version "2.3.22"

#(define ((futz-alignment-callback distance count) grob axis)

   "Check if we're the system number COUNT, and if yes, set fixed distance to
DISTANCE; then call the original callback.  "
   (let*
       ((a (ly:grob-parent grob axis))
	(o (ly:grob-original a))
	(bs (if (ly:grob? o)
		(ly:spanner-broken-into o)
		#f))
	)


     (if (and (list? bs)
	      (< count (length bs))
	      (equal? (list-ref bs count) a)
	 )
	 (ly:grob-set-property! a 'forced-distance distance))
     
     (Align_interface::fixed_distance_alignment_callback grob axis)) )

\score {
     \relative c''  \context PianoStaff
    \with {
	verticalAlignmentChildCallback = #(futz-alignment-callback 20 1)

	%% Every cross staff beam will trigger
	%% alignment unless autokneeing is switched off 
	\override Beam #'auto-knee-gap = #'()
    } <<

	\context Staff  = up {
	    
	    \time 2/4 
	    c8[
		\change Staff = down
		\once \override Stem #'direction = #UP
		c8
		\change Staff = up
		c c ](
	    |
	    \break
	    
	    c8[)
		\change Staff = down
		\once \override Stem #'direction = #UP
		c8
		\change Staff = up
		c c ](
	}
	\context Staff = down {
	    \skip 1 }

    >>
    \layout { raggedright = ##T } 
}
