\version "2.7.39"

\header{
    
    texidoc = "This should not hang lilypond --safe-mode run."

}

\score{
    % DOS
     c''-"\\loop\\iftrue\\repeat"
}
