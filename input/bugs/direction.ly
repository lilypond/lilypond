%
% urg ik dacht dat jij generic-properties en dir had gefikst?
% deze doet het wel in 1.3.37, voor je change + revert
%
\score{
\context Voice=x \notes\relative c''{
\property Voice.dynamicDirection = #-1
\property Voice.verticalDirection = #1

c\pp c c\< c \! c c \ff

}
\paper{
}
\midi{
\tempo 1 = 60;
}
}
