\version "1.7.18"
% regression or delete.  -gp
\header {texidoc="
REGRESSION or DELETE. "}

\score{
\notes\relative c''{

\slurUp
\stemDown
a(a' a, a-)
a(a a' a,-)
a(d a a-)
a(a d a-)

e'8(e e e e e e e-)
a,8(d a a a a a a-)
a(a a a a a d a-)

c,(g' c e f d b g-) 
c,(g' c e f d b g-) 

\property Voice.autoBeaming = ##f
\stemUp
a8(a a a a a a a-)

}
\paper{
indent=0.0\mm
}
}
%% new-chords-done %%
