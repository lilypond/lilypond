\version "2.6.0"

\header{
    
    texidoc = "This should not hang lilypond --safe-mode run."

}

\score{
    % DOS
     c''-"\\loop\\iftrue\\repeat"
}
