\version "2.25.5"

\header {
  texidoc = "Fingerings and bass figures in markup are scaled with the font size."
}

fingering = \markup {
  The scale fingering \finger 3 \finger 4 \finger 3 \finger 4 …
}

figuredbass = \markup {
  Symbols like \figured-bass "6\\" or \figured-bass 4+ are common.
}

\markup \fontsize #-3 \fingering
\markup \fontsize #0 \fingering
\markup \fontsize #3 \fingering
\markup \fontsize #6 \fingering
\markup \fontsize #9 \fingering

\markup \fontsize #-3 \figuredbass
\markup \fontsize #0 \figuredbass
\markup \fontsize #3 \figuredbass
\markup \fontsize #6 \figuredbass
\markup \fontsize #9 \figuredbass
