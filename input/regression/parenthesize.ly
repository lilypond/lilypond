
\header {


  texidoc = "The parenthesize function is a special tweak that
  encloses objects in parentheses. The associated grob is
  @code{Score.ParenthesesItem}." 
}


\paper {
  ragged-right = ##t
}

\version "2.7.32"

\relative {
  c4 -\parenthesize -.

  \override ParenthesesItem #'padding = #0.1
  <d \parenthesize fis a> 

}
