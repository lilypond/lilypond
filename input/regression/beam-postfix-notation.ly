
\version "2.1.22"
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
