\version "2.7.39"

fooBar =  { << c''4 \\ g'4 >> }

#(ly:set-option 'parse-protect #f)
#(load-from-path "to-xml.scm")

#(music-to-xml fooBar (current-output-port))

\header {
    texidoc = "@cindex To XML"
	texidoc = #(string-append
      "The input representation is very generic. Therefore, it
      should not be hard to convert it to XML or a similar format:\n\n"

      "@example\n"
	(call-with-output-string
	       (lambda (p) (music-to-xml fooBar p))
      )
    "@end example" )
}


\score {
\fooBar
}

