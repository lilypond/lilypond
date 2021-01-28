\version "2.21.80"

\header {
  texidoc = "Parts of a string that are the result of an automatic replacement
are not processed themselves for replacements."
}

\paper {
  #(add-text-replacements!
    '(
      ; Test that no replacements are performed after the start of
      ; the replacement string.
      ("This should not appear!" . "This is good.")
      ("good." . "BAD!")
      ; Test that replacements where the replacement is shorter than
      ; the replaced string are not subject to further replacements.
      ("This should not appear either!" . "This is shorter.")
      ("This is shorter." . "BAD!")
     ))
}

\markup "This should not appear!"
\markup "This should not appear either!"
