\header {


  texidoc = " The @code{cueDuring} form of quotation will set stem
directions on both quoted and main voice, and deliver the quoted voice
in the @code{cue} @code{Voice}. The music function @code{\killCues}
can be remove all cue notes."

	}
\version "2.5.0"
\layout {
  raggedright = ##t
}


quoteMe = \relative c' { fis4 r16  a8.-> b4-\ff  c4 }

\addquote quoteMe \quoteMe 

original = \relative c'' { c8 d s2 es8 gis8 }

cueStaff =  \relative c'' <<
				% setup cue note layout.
  \context Voice = cue  {
    \set fontSize = #-4
    \override Stem #'lengths = #'(2.5 2.5 3.0 3.0)
    \skip 1
  }
  
  \set Staff.quotedEventTypes = #'(note-event articulation-event)
  \original
  { s4 \cueDuring #"quoteMe"  #1 { r2 } }
>>

<<
  \new Staff {
    \set Staff.instrument = "quoteMe"
    \quoteMe
  }
  \new Staff {
    \set Staff.instrument = "orig"
    \original
  }
  \new Staff {
    \set Staff.instrument = "orig+quote"	
    \cueStaff
  }
  \new Staff {
    \set Staff.instrument = "killCues"	
    \killCues \cueStaff
    
  }
>>
