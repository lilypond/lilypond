
\header{
texidoc="
The first two a8 notes should not be beamed.
Also, no automatic beaming accross bar lines.
";
}

\score{
\notes \notes\relative c'' {
\time 2/8;
a8 a
\time 6/8;
a16 cis d a bes g fis4 g8
%a4. fis4 g8
a16 g a bes c d % ees8 d c
}
\paper{
   linewidth=-1.;
}
}
