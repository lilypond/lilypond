%{

The definition of markup is incomplete.

Ideally, either the input is valid, and all information therein is
used, or it is invalid, and an error message is produced.

%}


\score { \notes \relative c' {
    f_#'(lines "one" ( "tow"))

    % three ignored
    f_#'(lines "one" ( "two" "three" ))        

        % italic ignored.
    f_#'(lines "one" (bold italic "towo"))
    
  }}
