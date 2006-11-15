\version "2.10.0"

\header{
    
    texidoc = "This should not survive lilypond --safe-mode run, and
    certainly not write /tmp/safe-tex.tex"

    % beware
    % openout_any=y lilypond --keep --safe-mode -S latexoptions=']{article}
    % \let\nofiles\relax%' input/no-notation/safe-tex.ly

}

\score{
     c''-"\\newwrite\\barf\\immediate\\openout\\barf=/tmp/safe-tex.tex\\immediate\\write\\barf{hallo}"
}
