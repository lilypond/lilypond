
\version "2.3.17"
\header {

    texidoc = "@cindex Auto Beaming Override

The auto-beamer, which can be overridden, will only engrave beams 
that end before encountering of 
@itemize @bullet
@item  a rest,
@item
 an other, manually entered beam, or
@item
 a bar line. 
@end itemize

The @code{autoBeaming} can also be turned off.

"

}

%% TODO: check doc string. -hw

\score{
     \relative c''{
        #(override-auto-beam-setting '(end * * * *)  1 2)
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
	\set autoBeaming = ##f
      	c8 c c c
    }
    \paper{raggedright = ##t}
}


