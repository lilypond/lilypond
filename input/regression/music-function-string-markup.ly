\version "2.13.8"

\header {
  texidoc = "Music functions accept strings as markup arguments
when using the type predicate @code{markup?}
"
}

testFunc =
#(define-music-function (parser location text music) (markup? ly:music?)
   ;; dummy function, does nothing
   music)

\relative c' {
  \testFunc "test string"
  c2 <c \testFunc "test string" e>
}
