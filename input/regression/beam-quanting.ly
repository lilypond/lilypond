\header {

texidoc = "Beam quanting can be twiddled using grob
properties. The following example shows how Y and DY can be
unquantised and quantised to 0 and 4 respectively."

}

\version "1.5.68"

%{
Have some fun beam quanting
%}

% no y quantising
#(define (beam-vertical-position-quants m dy x y) '())

% rediculous dy quanting
#(define (beam-height-quants x y) '(0 4))

\score {
    \notes\relative c'{
        \property Voice.Beam \override #'height-quant-function = #beam-height-quants
        \property Voice.Beam \override #'vertical-position-quant-function =
	#beam-vertical-position-quants
        c8 c c c
        c8 e g a
        c,8 f b e
    }
}
