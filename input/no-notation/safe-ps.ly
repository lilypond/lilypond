\version "2.1.16"

\header{
    texidoc = "This should not survive lilypond run, and certainly not
    write /tmp/safe-ps.ps"
}

\score{
    \notes c''-"\\embeddedps{ (/tmp/safe-ps.ps) (w) file (hallo) writestring }"
   
}
