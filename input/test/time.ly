\version "1.5.68"
\header
{
    texidoc =  "
IMPORTANT NOTE: The current selection scheme for time signature
symbols is not flexible enough for future extensions such as various
flavours of early mensural notation or complex signatures as in
contemporary music.  Therefore, the semantics of time-signature
properties will quite definitely change, and maybe the syntax of the
\time request will possibly be extended. See the input file for TODOs.
    
"
}


%{

two examples what might be expected.  -- jr

TODO: The former "old6/8alt" is currently not addressable.  This will
be fixed by introducing an additional style property that switches
between various mensural diminution styles.  -- jr

TODO: Style "1xxx" really should be a special case of style
"numbered".  In other words, style "1xxx" should be removed, and a new
property "denominator-style" should be introduced, with values
"numbered" (which should be equivalent to the current "numbered"
style), "none" (which should be equivalent to the current "1xxx"
style), and "notehead" (which should place a proper notehead to the
right side of the nominator).  -- jr


%}

\score { 
  \context Voice \notes\relative c {
    % Lilypond doesn't understand 'default => it does what you want
    \property Staff.TimeSignature \override #'style = #'default
    \time 1/1
    c''1^"TimeSignature style = \#'default" 
    \time 2/2 
    c1 
    \time 2/4 
    c2 
    \time 4/8 
    c2 
    \time 3/4 
    c2. 
    \time 4/4 
    c1 
    \time 5/4 
    c2. c2 
    \time 6/4 
    c1. 
    \time 3/2 
    c1. 
    \time 7/4 
    c1 c2. 
    \time 8/4 
    c\breve
    \time 9/4 
    c2. c2. c2. 
    \break
    \property Staff.TimeSignature \set #'style = #'mensural
    \time 1/1 
    c1^"TimeSignature style = \#'mensural" 
    \time 2/2 
    c1 
    \time 2/4 
    c2 
    \time 4/8 
    c2 
    \time 3/4 
    c2. 
    \time 4/4 
    c1 
    \time 5/4 
    c2. c2 
    \time 6/4 
    c1. 
    \time 3/2 
    c1. 
    \time 7/4 
    c1 c2. 
    \time 8/4 
    c\breve 
    \time 9/4 
    c2. c2. c2. 
    \time 6/8 
    c2. 
    \time 9/8 
    c4. c4. c4. 
    \break
    \property Staff.TimeSignature \set #'style = #'neo_mensural
    \time 1/1 
    c1^"TimeSignature style = \#'neo\_mensural" 
    \time 2/2 
    c1 
    \time 2/4 
    c2 
    \time 4/8 
    c2 
    \time 3/4 
    c2. 
    \time 4/4 
    c1 
    \time 5/4 
    c2. c2 
    \time 6/4 
    c1. 
    \time 3/2 
    c1. 
    \time 7/4 
    c1 c2. 
    \time 8/4 
    c\breve 
    \time 9/4 
    c2. c2. c2. 
    \time 6/8 
    c2. 
    \time 9/8 
    c4. c4. c4. 
    \break
    \property Staff.TimeSignature \set #'style = #'numbered
    \time 1/1 
    c1^"TimeSignature style = \#'numbered"
    \time 2/2 
    c1
    \time 2/4 
    c2 
    \time 4/8 
    c2 
    \time 3/4 
    c2.
    \time 4/4 
    c1
    \time 5/4 
    c2. c2
    \time 6/4 
    c1.
    \time 3/2 
    c1.
    \time 7/4 
    c1 c2.
    \time 8/4 
    c\breve 
    \time 9/4 
    c2. c2. c2.
    \break
    % If the style starts with a '1', you get this style
    \property Staff.TimeSignature \set #'style = #'1style
    \time 1/1 
    c1^"TimeSignature style = \#'1xxx"
    \time 2/2 
    c1
    \time 2/4 
    c2 
    \time 4/8 
    c2 
    \time 3/4 
    c2.
    \time 4/4 
    c1
    \time 5/4 
    c2. c2
    \time 6/4 
    c1.
    \time 3/2 
    c1.
    \time 7/4 
    c1 c2.
    \time 8/4 
    c\breve 
    \time 9/4 
    c2. c2. c2. 
  }
  \paper { }  
  \midi { }
}
