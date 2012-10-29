
\header {


  texidoc = "The parenthesize function is a special tweak that
  encloses objects in parentheses. The associated grob is
  @code{Score.ParenthesesItem}." 
}


\paper {
  ragged-right = ##t
}

\version "2.17.6"

\relative c' {
  c4 -\parenthesize -.

  \override ParenthesesItem.padding = #0.1
  <d \parenthesize fis a> 

}
