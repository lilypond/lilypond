\version "2.2.0"

\header{
    
    texidoc = "This should not hang lilypond --safe-mode run."

}

\score{
    % DOS
    \notes c''-"\\loop\\iftrue\\repeat"
}
