\version "2.25.23"

\header {
  texidoc = "Slurs avoid @code{StrokeFinger} grobs with @code{avoid-slur}
set to @code{inside}; @code{StrokeFinger} grobs avoid slurs with
@code{avoid-slur} set to @code{around} (the default).  Slurs and
@code{StrokeFinger} grobs keep a distance as set by the
@code{slur-padding} property."
}

\relative c' {
  \set strokeFingerOrientations = #'(down)

  c4\rightHandFinger #2 b
  c4(\rightHandFinger #2 b)
  c4(\tweak slur-padding #2 \rightHandFinger #2 b)
  c4(\tweak avoid-slur #'inside \rightHandFinger #2 b)
}
