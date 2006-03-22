%% todo: move into advanced notation section of the manual.

\header {

  texidoc = "Time signatures may be put on a separate staff.  This is
  used contemporary pieces with many time signature changes.  "

}
\version "2.8.0"
\layout {
  ragged-right =  ##T
}

\layout{
  \context { 
    \type "Engraver_group"
    \consists "Time_signature_engraver"
    \consists "Axis_group_engraver"
    \name "TimeSig"
    \override TimeSignature #'font-size = #4
  }
  \context {
    \Score \accepts TimeSig
  }

  \context { \Staff
    \override TimeSignature #'transparent = ##t
  }
}


\relative
<< \new Staff { \time 2/4 c2 \time 3/4 c2. \time 4/4 c1 }
   \new TimeSig {
     \skip 4 * 9
   }
   \new Staff { r4 r r
		r4 r r
		r4 r r }

 >>
