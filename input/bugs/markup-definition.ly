\version "1.5.68"
\header {

texidoc="
The definition of markup is incomplete.

Ideally, either the input is valid, and all information therein is
used, or it is invalid, and an error message is produced."

}


\score { \notes \relative c' {
    f_#'(lines "one" ( "tow"))

    % FIXED:three ignored
    f_#'(lines "one" ( "two" "three" ))        

    % right way of using multiple markups
    f_#'(lines "one" ((bold italic) "towo"))
    
    % italic ignored.
    % possibly explicit relaxed code for chords stuff, must check
    f_#'(lines "one" (bold italic "towo"))
    
  }}
