
\version "2.3.4"

\header{
texidoc="@cindex Beam Over Rests
Beams may be forced to be over rests.
" }

\score{
    \relative c''{
	  r4  r8[ g a]
	   bes8[ r16 f g a]
	   bes8[ r16 \set stemLeftBeamCount = #1 f g a]
    }

    \paper{
        raggedright = ##t
    }
}

