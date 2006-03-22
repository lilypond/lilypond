\version "2.8.0"

\header{
    texidoc = "This should not survive lilypond --safe-mode
    --no-pdf --png run, and certainly not write /tmp/safe-ps.ps"
}

\score{
     c''-"\\embeddedps{ (/tmp/safe-ps.ps) (w) file (hallo) writestring }"
}
