\version "2.2.0"

\header{

    texidoc = "This should not hang lilypond --safe-mode --no-pdf
    --png run.

}

\score{
    % DOS
    \notes c''-"\\embeddedps{ { 0 0 moveto } loop }"
}
