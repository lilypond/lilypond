%
% empty bars?
%
\score{
\context Staff \notes\relative c''{
\property Staff.defaultBarType = #""
c1 c
\property Staff.defaultBarType = #"empty"
c1 c
}
\paper{
}
\midi{
\tempo 1 = 60;
}
}
