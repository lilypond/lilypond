\header {


  texidoc = " The @code{cueDuring} form of quotation will set stem
directions on both quoted and main voice, and deliver the quoted voice
in the @code{cue} @code{Voice}. The music function @code{\\killCues}
can remove all cue notes.

Spanners run to the end of a cue section, and are not started on the
last note."

}

\version "2.21.0"
\layout {
  ragged-right = ##t
}

quoteMe = \relative { fis'4 r16  a8.-> b4(-\ff~  b16 c8.  b) }

\addQuote quoteMe \quoteMe 

original = \relative {
  c''8 d
  \cueDuring "quoteMe"  #1 { r2 }
  es8 gis8
  \cueDuring "quoteMe"  #1 { r4 }
}

cueStaff =  \relative c'' <<
  \set Staff.quotedEventTypes = #'(note-event articulation-event)
  \original
>>

<<
  \new Staff {
    \set Staff.instrumentName = "quoteMe"
    \quoteMe
  }
  \new Staff {
    \set Staff.instrumentName = "orig (killCues)"
    \killCues \original
  }
  \new Staff {
    \set Staff.instrumentName = "orig+quote"	
    \cueStaff
  }
>>
