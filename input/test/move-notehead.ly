
\version "1.9.4"
\header { texidoc = "@cindex Scheme Move Notehead

You can move objects around with the property extra-offset.  This
example shows how to move noteheads around.  It uses the
@code{\outputproperty} command.

" }

fragment = \notes {
    \outputproperty #(make-type-checker 'note-head-interface)
      #'extra-offset = #'(2 . 3)
    c''2 c
}

\paper { raggedright = ##t} 

\score {
  \notes\relative c \fragment
  \paper { }  
}

