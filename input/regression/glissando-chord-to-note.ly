\version "2.27.2"

\header {
 texidoc = "A glissando from a chord to a single note does not disturb automatic
line breaking.
The expected output is a visible single glissando, with a proper line break
later."
}

%% I.e. check for #6946 "glissando and line break issue"

\relative { <c'' f>2.\glissando a'4 \repeat unfold 64 g16 }
