\version "1.7.18"
\header {
texidoc = "Test font selection and scm text markup"
}

\score{
  \notes\relative c''{
    \property Voice . TextScript \override #'font-shape = #'upright
    c1^\markup { \dynamic "p" "ma sosten." }  
    c^\markup \huge "ABCD" 
    \property Voice . TextScript \override #'font-series = #'bold
    c^\markup { \bold "Dal" " " \raise #0.8 \musicglyph #"scripts-segno" }
    c^\markup \huge "ABCD"
  }
}
%% new-chords-done %%
