%{
instrument names on grandstaffs. GR is centered on entire score
%}


\include "paper20.ly"
\header{
        latexpackages = "amsmath";
}

\score{
  \notes <
  \context Staff = sa {\property Staff.instrument = "A" g,2 c''''2}
  \context Staff = sb {\property Staff.instrument = "B" g,2 c''''2}
  \context GrandStaff=gs <
    \property GrandStaff.instrument = "Gr"
    \context Staff = ga {\property Staff.instrument = "" c''1 }
    \context Staff = gb {\property Staff.instrument = "" c''1 }
  >
  \context Staff = sc {\property Staff.instrument = "foobar"
% "\small$\mathrm{\genfrac{}{}{0pt}{0}{C}{D}}$"

<g'1 c''> }
  \context Staff = sd {\property Staff.instrument = "E" c''1 }

  >
  \paper{
    \translator { 
      \StaffContext
      \consists "Instrument_name_engraver";
    }
    \translator { 
      \GrandStaffContext
      \consists "Instrument_name_engraver";
    }

  }
}




