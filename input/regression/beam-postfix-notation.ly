#(ly:set-option 'old-relative)
\version "1.9.1"
\header {

texidoc = "Beams and ties may be entered in postfix notation, separating the
    notes and the brackets with a dash."

}
    \paper { raggedright= ##t }


\score
{
 \notes\relative c''
   {
       c8[~ c]
   }
}
