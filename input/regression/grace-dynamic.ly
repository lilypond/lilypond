\version "1.7.18"
\header{
 % ?
% niet zo onschuldig!  je hebt graces gefixt in 1.5, zie maar eens 1.3.150
texidoc="Dynamics on grace notes are small and behave nicely (don't crash into to main note)."
}
 
\score {
     \notes \relative c'  \context Voice {\grace {c_\f} d, c'  f g}
     \paper { raggedright =  ##t } 

}


