\score { 
  \context Voice \notes\relative c {
    \property Staff.TimeSignature \override #'style = #'C
    \time 1/1;
    c''1^"TimeSignature style = \#'C" 
    \time 2/2; 
    c1 
    \time 2/4; 
    c2 
    \time 3/4; 
    c2. 
    \time 4/4; 
    c1 
    \time 5/4; 
    c2. c2 
    \time 6/4; 
    c1. 
    \time 3/2; 
    c1. 
    \time 7/4; 
    c1 c2. 
    \time 8/4; 
    c\breve
    \time 9/4; 
    c2. c2. c2. 
    \break
    \property Staff.TimeSignature \set #'style = #'old
    \time 1/1; 
    c1^"TimeSignature style = \#'old" 
    \time 2/2; 
    c1 
    \time 2/4; 
    c2 
    \time 3/4; 
    c2. 
    \time 4/4; 
    c1 
    \time 5/4; 
    c2. c2 
    \time 6/4; 
    c1. 
    \time 3/2; 
    c1. 
    \time 7/4; 
    c1 c2. 
    \time 8/4; 
    c\breve 
    \time 9/4; 
    c2. c2. c2. 
    \time 6/8; 
    c2. 
    \time 9/8; 
    c4. c4. c4. 
    \break
    % Lilypond doesn't understand 'default => it does what you want
    \property Staff.TimeSignature \set #'style = #'default
    \time 1/1; 
    c1^"TimeSignature style = \#'default"
    \time 2/2; 
    c1
    \time 2/4; 
    c2 
    \time 3/4; 
    c2.
    \time 4/4; 
    c1
    \time 5/4; 
    c2. c2
    \time 6/4; 
    c1.
    \time 3/2; 
    c1.
    \time 7/4; 
    c1 c2.
    \time 8/4; 
    c\breve 
    \time 9/4; 
    c2. c2. c2.
    \break
    % If the style starts with a '1', you get this style
    \property Staff.TimeSignature \set #'style = #'1style
    \time 1/1; 
    c1^"TimeSignature style = \#'1xxx"
    \time 2/2; 
    c1
    \time 2/4; 
    c2 
    \time 3/4; 
    c2.
    \time 4/4; 
    c1
    \time 5/4; 
    c2. c2
    \time 6/4; 
    c1.
    \time 3/2; 
    c1.
    \time 7/4; 
    c1 c2.
    \time 8/4; 
    c\breve 
    \time 9/4; 
    c2. c2. c2. 
    \break
    \property Staff.TextScript \override #'self-alignment-X = #1
    \property Staff.TimeSignature \set #'style = #'old9/8
    \time 1/1; 
    c1^"old9/8" 
    \property Staff.TimeSignature \set #'style = #'old6/8
    \time 1/1; 
    c1^"old6/8" 
    \property Staff.TimeSignature \set #'style = #'old6/8alt
    \time 1/1; 
    c1^"old6/8alt" 
    \property Staff.TimeSignature \set #'style = #'old9/4
    \time 1/1; 
    c1^"old9/4" 
    \property Staff.TimeSignature \set #'style = #'old6/4
    \time 1/1; 
    c1^"old6/4" 
    \property Staff.TimeSignature \set #'style = #'old3/2
    \time 1/1; 
    c1^"old3/2" 
    \property Staff.TimeSignature \set #'style = #'old4/4
    \time 1/1; 
    c1^"old4/4" 
    \property Staff.TimeSignature \set #'style = #'old2/2
    \time 1/1; 
    c1^"old2/2"
    \property Staff.TimeSignature \set #'style = #'old2/4
    \time 1/1; 
    c1^"old2/4" 
    \property Staff.TimeSignature \set #'style = #'old4/8
    \time 1/1; 
    c1^"old4/8"
    \property Staff.TimeSignature \set #'style = #'C4/4
    \time 1/1; 
    c1^"C4/4" 
    \property Staff.TimeSignature \set #'style = #'C2/2
    \time 1/1; 
    c1^"C2/2" 
    
  }
  \paper { }  
  \midi { }
}
