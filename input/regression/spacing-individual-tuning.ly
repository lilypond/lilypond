\version "1.5.68"
\header {

texidoc = "

You can tune spacing of individual notes
by setting @code{space-factor} in @code{NoteSpacing}.

"
}

\score { \notes {
\relative c'' { 
c8 c8
\property Voice.NoteSpacing \set #'space-factor = #0.7
 c8 c8
\property Voice.NoteSpacing \set #'space-factor = #1.4
 c8 c8
\property Voice.NoteSpacing \set #'space-factor = #1.0
 c8 c8 
} }
\paper { linewidth = -1. }
}
