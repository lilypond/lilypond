% paper20-init.ly


\version "1.9.8"

paperTwenty = \paper {
	staffheight = 20.0\pt
	#(define fonts (scale-font-list  1.0))
	
	\include "params-init.ly"
}

\paper { \paperTwenty }


%{

; note:
; you can add fonts manually  in the paper block by issuing

#(set! fonts (append ...myfonts... fonts))

for the format of myfonts, see font.scm

%}
