\version "2.1.16"

\header{
    
    texidoc = "This should not hang lilypond --safe-mode run."

}

\score{
    % DOS
    \notes c''-"\\loop\\iftrue\\repeat"
}
