\score{
\context Staff \notes\relative c'{
c\sustaindown d e f\sustainup g\sustaindown b c
c,\sustainup\sustaindown d e f \sustainup g\sustaindown b c
\property Staff.stopStartSustain = #"-P"
\property Staff.startSustain = #"P"
c,\sustainup\sustaindown d e f \sustainup g\sustaindown b c
}
\paper{
}
\midi{
\tempo 4 = 60;
}
}
