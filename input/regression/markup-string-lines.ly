\version "2.23.0"

\header {
  texidoc = "The markup list command @code{\\string-lines} splits a given string
at line break characters and drops surrounding whitespace from the resulting
strings.
Other splitting points may be achieved by overriding the @code{split-char}
property.
"
}

\markup
  \column {
    \underline "All three instances should look equal!"
    \line {
      \column
        \box
        \string-lines
          "Verse \n Everywhere that Mary went \n The lamb was sure to go."

      \column
        \box
        \string-lines
          "Verse
           Everywhere that Mary went
           The lamb was sure to go."
      \column \box {
        "Verse"
        "Everywhere that Mary went"
        "The lamb was sure to go."
      }
    }
    \vspace #2
    \underline \line { Both lines should look equal! }
    \line \box { \override #'(split-char . #\/) \string-lines "aa/bb/cc/dd/ee" }
    \line \box { aa bb cc dd ee }
  }
