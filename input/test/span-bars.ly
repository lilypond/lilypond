
\version "1.0.14";
nt = \notes { c1 \break c1 c1 } 
stuff = \notes \relative c'' <
  \type Staff = stone  { \nt }
  \type Staff = sttwo { \nt }
>

\score{ 
    \type StaffGroup \stuff
}

\score{ 
    \type StaffGroup < \type GrandStaff \stuff
        \type Staff = stthree \nt 
    >
}

\score{ 
    \type ChoirStaff \stuff
}


