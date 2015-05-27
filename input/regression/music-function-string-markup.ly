\version "2.19.22"

\header {
  texidoc = "Music functions accept strings as markup arguments
when using the type predicate @code{markup?}
"
}

testFunc =
#(define-music-function (text music) (markup? ly:music?)
   ;; dummy function, does nothing
   music)

\relative c' {
  \testFunc "test string"
  c2 <c \testFunc "test string" e>
}
