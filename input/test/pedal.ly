\score{
\context Staff \notes\relative c'{
c4\sustaindown d e f\sustainup g\sustaindown b c
c, [d16 \sustainup \sustaindown c  c c] [e e \sustainup \sustaindown e e ] f4 \sustainup g\sustaindown b c
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
