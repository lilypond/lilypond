\version "1.7.19"
\header {
texidoc="Multi measure rests of second voice should not disappear."
}


%{

TODO:  what does this test? The contexts are tweaked too much.

%}

#(ly:set-point-and-click 'line-column)
one = \notes\relative c'' {

	f4 e-\!-.-\f r2|
	R1|
	f2-\p-(es -)|
	d1\<|
	d1|
	d2 d2|
	d2 d2|
	f1-\!-\ff  |
	f1
}

two = \notes \relative c'' {
	r4 c r2|
	R1*6|
	c1 ~|
	c1
}
	
\score {
  \notes <
    \context Staff = Viole <
	\context Voice=one \partcombine Voice
		\context Thread=one \one
		\context Thread=two \two
    >
  >
  \paper {
    \translator {
      \ThreadContext
      \consists "Rest_engraver"
    }
    \translator {
      \VoiceContext
      \remove "Rest_engraver"
      \consists Multi_measure_rest_engraver
      \consists Bar_engraver
    }
    \translator {
      \RemoveEmptyStaffContext
      \remove Multi_measure_rest_engraver
      \remove Bar_engraver
    }
    \translator {
      \OrchestralScoreContext
      RestCollision \override #'maximum-rest-count = #1
    }
  }
}
