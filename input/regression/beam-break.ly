
\header {
    texidoc = "Beams can be printed across line breaks if forced.
"

}
\version "1.9.8"
    \paper { raggedright= ##t }

\score { \notes \relative c''  {
    \property Score.forbidBeamBreak = ##f
   \time 3/16 c16-[ d e \break f-] 
}}
