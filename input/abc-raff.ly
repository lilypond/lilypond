
voicedefault = \notes {
\time 6/8; \key D;   d''8    fis''8    fis''8    cis''8    e''8    e''8  |
   d''8    e''8    fis''8    g''8    fis''8    e''8  |
   d''8    fis''8    fis''8    cis''8    e''8    e''8  |
   d''8    fis''8    e''8    d''8    b'8    a'8  |
   d''8    fis''8    fis''8    cis''8    e''8    e''8  |
   d''8    e''8    fis''8    g''8    fis''8    e''8  |
   fis''8    a''8    fis''8    g''8    fis''8    e''8  \bar "|";   d''8    
fis''8    e''8    d''8    b'8    a'8  \bar ":|";   d''8    fis''8    e''8    
d''8    cis''8    b'8  \bar "||";     a'4. -\trill   b'4.  |
   g''8    fis''8    e''8    fis''8    d''8    b'8  |
   a'8    fis'8    a'8    b'4    cis''8  |
   d''8    fis''8    e''8    d''8    cis''8    b'8  |
   a'4. -\trill   b'4. -\trill |
   e''8    fis''8    e''8    e''8    fis''8    g''8  |
   fis''8    a''8    fis''8    g''8    fis''8    e''8  \bar "|";   d''8    
fis''8    e''8    d''8    cis''8    b'8  \bar ":|";   d''8    fis''8    e''8    
d''8    b'8    a'8  \bar "||";     fis''8    a'8    a'8    e''8    a'8    a'8  
|
   d''8    e''8    fis''8    g''8    fis''8    e''8  |
   fis''8    a'8    a'8    e''8    a'8    a'8  |
   d''8    fis''8    e''8    d''8    b'8    a'8  |
   fis''8    a'8    a'8    e''8    a'8    a'8  |
   d''8    e''8    fis''8    g''8    fis''8    e''8  |
   fis''8    a''8    fis''8    g''8    fis''8    e''8  |
   d''8    fis''8    e''8    d''8    b'8    a'8  \bar ":|";   
}\score{
        \notes <

        \context Staff="default" \$voicedefault 
    >\header {
composer = "Trad.";

tagline = "Lily was here -- Directly converted from ABC";

title = "Paddy O'Rafferty";
}
\paper {}
\midi {}
}