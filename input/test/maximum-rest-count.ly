\score{
\context Staff \notes\relative c''{
< a4 c e >
< r r r >
\property Staff.maximumRestCount = #3
< r r r >
\property Staff.maximumRestCount = #2
< r r r >
\property Staff.maximumRestCount = #1
< r r r >
\property Staff.maximumRestCount = #0
< r r r >
% urg
r
}
\paper{
}
\midi{
\tempo 1 = 60;
}
}
