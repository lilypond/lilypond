\version "2.25.19"

\header {
  texidoc = "The @code{FingerGlideSpanner} is able to connect non-matching
@code{Fingering}s using the @code{id} property.
In the test, equally colored fingers should be connected."
}

{
  b2 \glide \iId #'foo \tweak color #red ^1
     \glide \iId #'bar \tweak color #green ^1
     \glide \tweak color #magenta _2
     \glide \tweak color #cyan _1
  b' \iId #'foo \tweak color #red ^2
     \iId #'bar \tweak color #green ^1
     \tweak color #magenta _2
     \tweak color #cyan _1

  \set fingeringOrientations = #'(up)
  <
   b\glide \iId #'foo \tweak color #red -1
   f'\glide \iId #'bar \tweak color #cyan -2
   d''\glide \iId #'buzz \tweak color #magenta -3
   b''\glide \iId #'blub \tweak color #green -4
  >
  <
   b\iId #'blub \tweak color #green -5
   f'\iId #'buzz \tweak color #magenta -6
   d''\iId #'bar \tweak color #cyan -7
   b''\iId #'foo \tweak color #red -8
  >
}
