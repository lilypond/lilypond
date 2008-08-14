
\header {
  texidoc = "Users may define non-standard markup commands using
the @code{define-markup-command} scheme macro."
}


\layout { ragged-right = ##t }
  


\version "2.11.51"

#(define-markup-command (upcase paper props str) (string?)
  "Upcase the string characters. Syntax: \\upcase #\"string\""
  (interpret-markup paper props (make-simple-markup (string-upcase str))))


{ 
  c''-\markup \upcase #"hello world"
				% produces a "HELLO WORLD" markup
}


