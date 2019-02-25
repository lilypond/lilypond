
\header {

  texidoc = "The @code{switchInstrument} music function modifies
properties for an in staff instrument switch. "
  }

\version "2.21.0"
\addInstrumentDefinition "bassClar"
  #`((instrumentTransposition . ,(ly:make-pitch -1 6 FLAT))
     (instrumentName . "bla") 
     (shortInstrumentName . "bl")
     (clefGlyph . "clefs.F") 
     (middleCPosition . 6)
     (clefPosition . 2)
     (instrumentCueName . ,(make-bold-markup "cl. B"))
     (midiInstrument . "clarinet"))


\paper {
  ragged-right = ##t
}

\relative
{
  c'4
  \instrumentSwitch "bassClar"
  c2.\break
  c1\break
  c
}
