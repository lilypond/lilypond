\version "1.7.3"

fooBar = \notes { < c''4 \\ g'4 > }

#(ly:set-parse-protect #f)
#(load-from-path "to-xml.scm")

#(music-to-xml fooBar (current-output-port))

\header {
    texidoc =

    #(string-append
      "The input representation is very generic. It
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
