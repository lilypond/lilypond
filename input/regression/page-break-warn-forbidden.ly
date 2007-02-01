
\header {

  texidoc = "If a page break is forced where it is forbidden,
 a warning is printed."

}

\version "2.11.14"

\new Staff {
   c'1 \glissando
   \pageBreak
   d'1
}

