\header {
  texidoc="Document trills, pralls and turns"
  title="Marques des agr\'ements et leur signification"
  copyright="(1689)"
}

\score {
  <
    \property Score.TimeSignature = \turnOff
    \context GrandStaff <
      \context Staff=upper \notes\relative c'' {
	\time 1/4
	c4-\prallprall
	\time 3/8
	c4.^"TODO"
	c4.-\downprall
	c4.-\upprall
	\time 5/8
	c4-\turn c4.-\upprall
      }
      \context Lyrics=one \lyrics {
	"Tremblement"4
	"Tremblement"4.
	"Cadence"
	"autre"
	"Double"4 "cadence"4.
      }
      \context Lyrics=two \lyrics {
	"simple"4
	"appuy\'e"4.
      }
      \context Staff=lower \notes\relative c'' {
        % autobeamer has som problems here
	[\repeat unfold 4 { d32 c }]
	d8~[\repeat unfold 4 { d32 c }]
	d32 c b c \repeat unfold 4 { d32 c }
	b32 c d c \repeat unfold 4 { d32 c }
	[c32( b a16 b )c] [b32 c d c \repeat unfold 4 { d32 c }]
      }
    >
  >
}