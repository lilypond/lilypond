
\version "2.3.4"
\header {

texidoc = "Beams and ties may be entered in postfix notation, separating the
    notes and the brackets with a dash."

}
    \paper { raggedright= ##t }


\score
{
 \relative c''
   {
       c8[~ c]
   }
}
