\version "1.3.146"


%
% Test font selection and scm text markup
%

\score{
  \notes\relative c''{
    c'1^#'(rows (dynamic "p") " ma sosten.") 
    c^#'(bold "ABCD")
    c^#'(rows "Dal " (music "scripts-segno")) 
    c^#'(Large "ABCD") 
    \break
    \property Voice . TextScript \override #'font-shape = #'upright
    c1^#'(rows (dynamic "p") " ma sosten.")  
    c^#'(bold "ABCD")
    \property Voice . TextScript \override #'font-series = #'bold
    c^#'(rows "Dal " (music "scripts-segno")) 
    c^#'(Large "ABCD")
  }
}
