\header {

  texidoc = "A harmonic note head must be centered if the base note is a whole
note.  The lower staff shows how the same chords without harmonic heads are
engraved."
}


\version "2.19.21"

\paper {
  ragged-right = ##t
}

testHarmonic = \tag #'actual -\harmonic

testMusic = \fixed c' {
  \time 4/1
  <e\testHarmonic a>1
  <e a\testHarmonic>1
  <e'' a''\testHarmonic>1
  << <f b\testHarmonic e'>1 \\ <f b\testHarmonic e'>1 >>
  %% voices not crossed
  << <e a\testHarmonic>1 \\ d >>
  << <e\testHarmonic a>1 \\ d >>
  << b1 \\ <e a\testHarmonic> >>
  << b1 \\ <e\testHarmonic a> >>
  %% voices crossed
  << d1 \\ <e a\testHarmonic> >>
  << d1 \\ <e\testHarmonic a> >>
  << <e a\testHarmonic>1 \\ b >>
  << <e\testHarmonic a>1 \\ b >>
}

<<
  \new Staff { \testMusic  }
  \new Staff { \removeWithTag #'actual \testMusic }
>>
