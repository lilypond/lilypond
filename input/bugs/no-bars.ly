\header{
texidoc="
defaultBarType is cheched by Timing_translator, but has no effect?
";
}

\score {
  \notes \relative c'' {
    a b c d
    d c b a
  }
  \paper {
    \translator {
      \StaffContext
      defaultBarType = #"" 
      \remove "Time_signature_engraver";
      linewidth = -1.;
    }
  }
}
