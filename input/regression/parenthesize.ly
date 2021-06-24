
\header {


  texidoc = "The parenthesize function is a special tweak that
  encloses objects in parentheses. The associated grob is
  @code{Score.Parentheses}."
}


\paper {
  ragged-right = ##t
}

\version "2.23.4"

\relative {
  c'2 -\parenthesize -.

  \parenthesize \breathe

  \override Parentheses.padding = #0.1
  <d \parenthesize fis a> 

}
