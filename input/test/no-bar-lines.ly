\version "1.7.18"

\score {
  \notes \relative c'' {
    a b c d
    d c b a
  }
  \paper {
    raggedright = ##t
    \translator {
      \StaffContext
      whichBar = #""
      \remove "Time_signature_engraver"
    }
  }
}

%% new-chords-done %%
