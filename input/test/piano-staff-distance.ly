
\header
{

 texidoc = "It is possible to have different staff distances across
piano systems, but it requires some advanced magic. Kids don't try this at home.

"

}

\version "2.1.24"

#(define ((futz-alignment-callback distance count) grob axis)

   "Check if we're the system number COUNT, and if yes, set fixed distance to
DISTANCE; then call the original callback.  "
   (let*
       ((a (ly:get-parent grob axis))
	(o (ly:get-original a))
	(bs (if (ly:grob? o)
		(ly:get-broken-into o)
		#f))
	)


     (if (and (list? bs)
	      (< count (length bs))
	      (equal? (list-ref bs count) a)
	 )
	 (ly:set-grob-property! a 'forced-distance distance))
     
     (Align_interface::fixed_distance_alignment_callback grob axis)) )

\score {
    \notes \relative c''  \context PianoStaff
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
    \paper { raggedright = ##T } 
}
