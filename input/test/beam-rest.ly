
\version "2.1.22"

\header{
texidoc="@cindex Beam Over Rests
Beams over rests.
" }

\score{
    \notes\relative c''{
	  r4  r8[ g a]
	   bes8[ r16 f g a]
	   bes8[ r16 \set stemLeftBeamCount =  #1 f g a]
    }

    \paper{
        raggedright = ##t
    }
}

