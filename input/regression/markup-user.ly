
\header {

  texidoc = "Own markup commands may be defined by using the
    @code{def-markup-command} scheme macro."


}


\layout { raggedright = ##t }
  


\version "2.7.13" % to be updated

#(def-markup-command (upcase paper props str) (string?)
  "Upcase the string characters. Syntax: \\upcase #\"string\""
  (interpret-markup paper props (make-simple-markup (string-upcase str))))


{ 
  c''-\markup \upcase #"hello world"
				% produces a "HELLO WORLD" markup
}


