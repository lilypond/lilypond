\version "2.1.1"
% JUNKME.

%% deprecated
papersizename = \papersize 

% DO NOT change this without fixing -f ps output
papersize = \papersize

% FIXME
% direct PostScript line height for single line staves
lineheight = 14

paperfile = \papersize + "-init.ly"

% paperfile = "a4-init.ly"

\include \paperfile
\include "paper-init.ly"

staffspace = #(/ staffheight 4.0)
linethickness = 0.5 \pt 
outputscale =  #(/ staffheight 4.0)

% don't do full 2x for ledger, otherwise no white is left. 
ledgerlinethickness = #(+ linethickness (/ staffspace 10))

% 2/3 stafflinethickness in 20pt staffheight
% this parameter is independent of the output size.
blotdiameter = 0.35 \pt
interscoreline = 4. \mm




%%
%% TODO: baseline-skip, word-space should come from the font.
%%
#(define font-defaults
      '((font-family . music)
	(font-shape . upright)
	(baseline-skip . 2)
	(word-space . 0.6)
	(font-series . medium)
	))



\include "engraver-init.ly"
