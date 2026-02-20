\version "2.25.34"

\header {
  texidoc="A @code{\\header} block used as a function argument does not carry
all previously defined variables, but only those that appear in the block.  The
output should have both ``Top-level Title'' and ``Score Title'' titles and a
``Modification'' subtitle."
  title = "Top-level Header"
}

#(ly:set-option 'warning-as-error #t)

\paper {
  print-all-headers = ##t
}

passHeader =
#(define-scheme-function (module) (module?)
  module)

headerMod = \passHeader \header {
  subtitle = "Modification"
}

\score {
  \header {
    title = "Score Header"
    %% In older versions, this \headerMod reassigned title = "Top-level Header".
    \headerMod
  }
  { b'1 }
}
