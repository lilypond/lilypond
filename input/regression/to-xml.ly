\version "2.23.2"

testMusic =  { << c''4 \\ g'4 >> }

#(use-modules (lily to-xml))

#(ly:progress "\nXML:\n\n~A\n" (call-with-output-string (lambda (p) (music-to-xml testMusic p))))


\header {
  texidoc =
  "The input representation is generic, and may be translated to XML. "
}


{ \testMusic }
