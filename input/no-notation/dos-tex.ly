\version "2.3.22"

\header{
    
    texidoc = "This should not hang lilypond --safe-mode run."

}

\score{
    % DOS
     c''-"\\loop\\iftrue\\repeat"
}
