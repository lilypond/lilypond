\header {

    texidoc = "This file demonstrates how to load different
    (postscript) fonts. The file @file{font.scm} shows how to define
    the scheme-function @code{make-century-schoolbook-tree}.

    This file should be run with the TeX and extra options should be
    passed to LaTeX and dvips to help it find the uncb font."

}
\version "2.11.51"

\paper
{
    #(define text-font-defaults
      '((font-encoding . latin1)
	(baseline-skip . 2)
	(word-space . 0.6)))

    #(set! fonts (make-century-schoolbook-tree 1.0))
}

%ugh.
% do this here so we don't forget the connection with
% this file.
#(system "afm2tfm `kpsewhich uncb8a.afm` uncb8a.tfm") 

\layout {
    line-width = 160 \mm - 2.0 * 9.0 \mm

    
    indent = 0.0\mm
    ragged-right = ##t
}

{
    \key a \major
    \time 6/8
    cis''8.

%% uncomment to test postscript fonts. 
% ^"test!"

    
    
    d''16 cis''8 e''4 e''8
}
