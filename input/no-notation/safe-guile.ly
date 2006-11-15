\version "2.10.0"

\header{
    
    texidoc = "This should not survive lilypond --safe-mode, and
    certainly not write /tmp/safe-guile.scm"
    
}

#(write "hallo" (open-file "/tmp/safe-guile.scm" "w"))

\score{
     c''
}