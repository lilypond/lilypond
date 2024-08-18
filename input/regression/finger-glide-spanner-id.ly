\version "2.25.19"

\header {
  texidoc = "The @code{FingerGlideSpanner} is able to connect non-matching
@code{Fingering}s using the @code{id} property.
In the test, equally colored fingers should be connected."
}

{
  b2 \glide \= #'foo \tweak color #red ^1
     \glide \= #'bar \tweak color #green ^1
     \glide \tweak color #magenta _2
     \glide \tweak color #cyan _1
  b' \= #'foo \tweak color #red ^2
     \= #'bar \tweak color #green ^1
     \tweak color #magenta _2
     \tweak color #cyan _1

  \set fingeringOrientations = #'(up)
  <
   b\glide \=1 \tweak color #red -1
   f'\glide \=2 \tweak color #cyan -2
   d''\glide \=3 \tweak color #magenta -3
   b''\glide \=4 \tweak color #green -4
  >
  <
   b\=4 \tweak color #green -5
   f'\=3 \tweak color #magenta -6
   d''\=2 \tweak color #cyan -7
   b''\= #1 \tweak color #red -8
  >
}
