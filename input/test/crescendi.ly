\score{
\notes\relative c''{
a1\fff\< \!a
a\> \!a
\property Voice.crescendoText = "cresc."
\property Voice.crescendoSpanner = "dashed-line"
a\mf\cresc \endcresc a
%a\decresc \enddecresc a
a1\< \!a
a\> \!a
}
\paper{
}
\midi{
\tempo 1 = 60;
}
}
