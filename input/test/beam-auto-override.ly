
\version "1.9.8"
\header {

    texidoc = "@cindex Auto Beaming Override

This shows how auto-beaming settings can be overridden.

The auto-beamer will only engrave beams that end when:
@itemize @bullet
@item  a rest is encountered
@item
 another beam (entered manually) is encountered
@item
 there's a 'gap' in the beam note's durations
@end itemize

The beam will be ended also when now % beamAutoEnd = 0.

"

}

%% TODO: check doc string. -hw

\score{
    \notes \relative c''{
    	\time 2/4
	% one beam per measure
      	c8 c c c
      	c16 c c c c c c c
	% from here on consider ending beam every 1/4 note
	#(override-auto-beam-setting '(end * * * *) 1 4)

      	c8 c c c
	% manually override autobeam with weird beaming
      	c8  c[ c] c
      	c8 c c r
      	c8 c c4
      	r8 c c c
	% no autobeaming
	\property Voice.autoBeaming = ##f
      	c8 c c c
    }
    \paper{
      	\translator{
	    \VoiceContext
	    autoBeamSettings \override #'(end * * * *) = #(ly:make-moment 1 2)
	}
    }
\paper{raggedright = ##t}
}


