
\version "2.1.23"

\header {

texidoc = "In tight situations, hyphens are removed, except at the
end of the line.  Normally, lyrics aren't set this tight, but by
tuning down @code{padding} of in @code{SeparationItem}, syllables are put closer together, and hyphens may disappear.

In some languages (eg. German and Hungarian).  hyphens should not
disappear, since spelling depends on hyphenation. In this case,
hyphens can be forced to remain by setting @code{minimum-length} on
the LyricHyphen grob.
"

}

\score {
<<    \notes \new Staff \relative c'' { \time 1/4 c16[ c c  c]
\time 1/4
c16[ c c c]
\time 1/4
c16[ c c c]

}
    \lyrics \new Lyrics \with {
	% Otherwise lyrics are so far apart that hyphens don't disappear
	\override SeparationItem #'padding = #0.0
	}{ bla -- bla -- bla -- bla --
	   bla -- bla -- bla -- bla --

	   \override LyricHyphen  #'minimum-length = #0.7
	   \override LyricHyphen  #'spacing-procedure =
                  #Hyphen_spanner::set_spacing_rods

	   bla -- bla -- bla -- bla 
       }>>
    \paper   {
	indent = 0.0 \cm
	linewidth = 3.4 \cm

	\translator {
	    \StaffContext \remove "Time_signature_engraver"
	}
	
    }
      
}

	
