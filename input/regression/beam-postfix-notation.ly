
\version "2.2.0"
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
