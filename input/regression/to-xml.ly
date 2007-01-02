\version "2.10.0"

testMusic =  { << c''4 \\ g'4 >> }

#(load-from-path "to-xml.scm")

#(ly:progress "\nXML:\n\n~A\n"
  (call-with-output-string
   (lambda (p) (music-to-xml testMusic p))))

\header {
  texidoc =
  "The input representation is generic, and may be translated to XML. "
}


\testMusic

