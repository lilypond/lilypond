\header {
texidoc="MIDI and midi2ly test file.  Diff between this and midi2ly.py (experimental, 1.5.17) should be minimal"
}

scales = \notes {

  % [INSTRUMENT_NAME] bright acoustic
  \key c \major  % sharp-major
  c4 d4 e4 f4 g4 a4 b4 c4 

  \key g \major
  g4 a4 b4 c4 d4 e4 fis4 g4 

  \key d \major
  d4 e4 fis4 g4 a4 b4 cis4 d4 

  \key a \major
  a4 b4 cis4 d4 e4 fis4 gis4 a4 

  \key e \major
  e4 fis4 gis4 a4 b4 cis4 dis4 e4 

  \key b \major
  b4 cis4 dis4 e4 fis4 gis4 ais4 b4 

  \key fis \major
  fis4 gis4 ais4 b4 cis4 dis4 eis4 fis4 

  \key cis \major
  cis4 dis4 eis4 fis4 gis4 ais4 bis4 cis4 

  \key a \minor  % sharp-minor
  a4 b4 c4 d4 e4 f4 gis4 a4 

  \key e \minor
  e4 fis4 g4 a4 b4 c4 dis4 e4 

  \key b \minor
  b4 cis4 d4 e4 fis4 g4 ais4 b4 

  \key fis \minor
  fis4 gis4 a4 b4 cis4 d4 eis4 fis4 

  \key cis \minor
  cis4 dis4 e4 fis4 gis4 a4 bis4 cis4 

  \key gis \minor
  gis4 ais4 b4 cis4 dis4 e4 fisis4 gis4 

  \key dis \minor
  dis4 eis4 fis4 gis4 ais4 b4 cisis4 dis4 

  \key ais \minor
  ais4 bis4 cis4 dis4 eis4 fis4 gisis4 ais4 

  \key f \major  % flat-major
  f4 g4 a4 bes4 c4 d4 e4 f4 

  \key bes \major
  bes4 c4 d4 ees4 f4 g4 a4 bes4 

  \key ees \major
  ees4 f4 g4 aes4 bes4 c4 d4 ees4 

  \key aes \major
  aes4 bes4 c4 des4 ees4 f4 g4 aes4 

  \key des \major
  des4 ees4 f4 ges4 aes4 bes4 c4 des4 

  \key ges \major
  ges4 aes4 bes4 ces'4 des4 ees4 f4 ges4 

  \key ces \major
  ces'4 des4 ees4 fes4 ges4 aes4 bes4 ces'4 

  \key d \minor  % flat-minor
  d4 e4 f4 g4 a4 bes4 cis4 d4 

  \key g \minor
  g4 a4 bes4 c4 d4 ees4 fis4 g4 

  \key c \minor
  c4 d4 ees4 f4 g4 aes4 b4 c4 

  \key f \minor
  f4 g4 aes4 bes4 c4 des4 e4 f4 

  \key bes \minor
  bes4 c4 des4 ees4 f4 ges4 a4 bes4 

  \key ees \minor
  ees4 f4 ges4 aes4 bes4 ces'4 d4 ees4 

  \key aes \minor
  aes4 bes4 ces'4 des4 ees4 fes4 g4 aes4 
}

\score {
  \context Voice \scales
  \paper { }
  \midi { }
}
