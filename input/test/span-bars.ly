\version "1.7.18"

\header { texidoc = "I really don't understand this one.  DELETE or REGRESSION? -gp " }

nt =  \notes { c1 \break c1 c1 } 
stuff =  \notes \relative c'' <
  \context Staff = stone  { \nt }
  \context Staff = sttwo { \nt }
>

\score{ 
    \context StaffGroup \stuff
}

\score{ 
    \context StaffGroup < \context GrandStaff \stuff
        \context Staff = stthree \nt 
    >
}

\score{ 
    \context ChoirStaff \stuff
}


%% new-chords-done %%
