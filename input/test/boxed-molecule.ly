
\version "2.1.21"
\header {
texidoc = "@cindex Boxed Molecule

You can override the print-function to draw a box around arbitrary grobs. " }


 \score { \notes \relative c''  {

 \property Voice.TextScript \override #'print-function =
   #(make-molecule-boxer 0.1 0.3 0.2 brew-new-markup-molecule)

   c'4^"foo"

\property Voice.Stem \override #'print-function =
   #(make-molecule-boxer 0.05 0.25 0.25 Stem::brew_molecule)

\property Score.RehearsalMark \override #'print-function =
   #(make-molecule-boxer 0.15 0.3 0.3 brew-new-markup-molecule)
      c8
\property Voice.Stem \revert #'print-function

      c4. c4 \mark "F" c1 
   }
\paper{raggedright = ##t}
}

