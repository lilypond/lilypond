
\version "2.3.22"
\header {

texidoc = "Beams and ties may be entered in postfix notation, separating the
    notes and the brackets with a dash."

}
    \layout { raggedright= ##t }


\score
{
 \relative c''
   {
       c8[~ c]
   }
}
