\version "2.19.21"

\header {
  texidoc = "The PDF backend uses several header fields to store metadata
in the resulting PDF file. Header fields with the prefix pdf override
those without the prefix for PDF creation (not for visual display on the page).
"

  title = \markup { \italic "Title of " \bold \concat {"the" " " \abs-fontsize #27 "piece"} }
  subtitle = \markup { \concat { "Subtitle" " of" " the" " " \natural "piece" }}
  composer = \markup { \bold \concat {"The" " " "Genius" " " "Composer"}}
  pdfcomposer = "Composer used for PDF"
  arranger = \markup { The Arranger \circle f \draw-circle #3 #0.5  ##t }
  copyright = "The Copyright"
  keywords = "pdfmark, metadata, DOCINFO, lilypond"
  modDate = "Test for special characters : \ ) ("
}
\layout { ragged-right= ##t }

\relative { g4 }
