\version "1.5.68"
\header{

texidoc="
Multiple measure rests do not collide with barlines and clefs.  They
are not expanded when you set @code{Score.skipBars}.  Although the
multi-measure-rest is a Spanner, minimum distances are set to keep it
colliding from barlines. 

Texts may be added to the rests by setting @code{text} in
@code{MultiMeasureRestNumber.}

Rests over measures during longer than 2 wholes use breve rests.
"

}


\score { \notes {
    \time 3/4 \key cis \major
    R2.*15
    R2.
 \once \property Staff.MultiMeasureRestNumber \set #'text = #'(music "scripts-ufermata")
    R2.
    R2.*3
    R2.*7
\time 8/4 R1*2
}
	\paper {
	\translator {
		\ScoreContext
		skipBars = ##t
	}
	linewidth = -1.	
	}
}
