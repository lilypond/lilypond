
\version "2.3.4"

\header {

texidoc = "In tightly engraved music, hyphens are removed, except at the
end of the line.  Normally, lyrics are not typeset so tightly, but by
tuning down @code{padding} of in @code{SeparationItem}, syllables are put 
closer together, and as a result hyphens may disappear.

In some languages (e.g. German and Hungarian),  hyphens should not
disappear, since spelling depends on hyphenation. For that purpose,
hyphens can be forced to remain by overriding @code{minimum-length} of
the @code{LyricHyphen} grob.
"

}

\score {
<<     \new Staff \relative c'' { \time 1/4 c16[ c c  c]
\time 1/4
c16[ c c c]
\time 1/4
c16[ c c c]

}
      \new Lyrics \with {
	% Otherwise lyrics are so far apart that hyphens don't disappear
	  \override SeparationItem #'padding = #0.0
      }
      \lyrics {
	  bla -- bla -- bla -- bla --
	  bla -- bla -- bla -- bla --

	  \override LyricHyphen  #'minimum-length = #0.7
	  \override LyricHyphen  #'spacing-procedure =
	  #Hyphen_spanner::set_spacing_rods

	   bla -- bla -- bla -- bla 
       }>>
    \paper   {
	indent = 0.0 \cm
	linewidth = 3.4 \cm

	\context {
	    \Staff \remove "Time_signature_engraver"
	}
	
    }
      
}

	
