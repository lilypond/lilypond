\version "2.11.51"

\header {

  texidoc = "Ledger lines are shortened when there are accidentals.
This happens only for the single ledger line close to the note head,
and only if the accidental is horizontally close to the head.  "
  
}


\relative c {
  disis
  dis
  d
  des
  deses
  eses
  es
  e
  eis
  eisis
  eses'''
  es
  e
  eis
  eisis
  feses
  fes
  f
  fis
  fisis
  <cis,, gis'>
  
} 

