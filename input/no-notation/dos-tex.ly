\version "2.1.36"

\header{
    
    texidoc = "This should not hang lilypond --safe-mode run."

}

\score{
    % DOS
    \notes c''-"\\loop\\iftrue\\repeat"
}
