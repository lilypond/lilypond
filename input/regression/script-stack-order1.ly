\version "2.25.1"

\header {
 texidoc = "The vertical stacking order of scripts goes from least
to most technical, where the least technical scripts are closest to
the note: articulation, flageolet, fingering, right-hand fingering,
string number, fermata, bowing, text script."
}

{
  d''16\3-3
  d'\5-4
}

\relative {
  e''4->-0\downbow ( c4-.) d--\downbow d,-.-0->
  r4
  a-.--\upbow f''-.---3\upbow e'\flageolet\fermata\upbow
  e,---0\downbow^"div."
}
