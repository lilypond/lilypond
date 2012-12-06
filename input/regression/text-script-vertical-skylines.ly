\version "2.16.0"

\header {
  texidoc = "By default, @code{TextScript} vertical skylines allow
for stack @code{TextScript} grobs to fit snugly over each other instead
of moving the entire distance of the bounding box.
"
}

\relative c' {
  a^\markup { \filled-box #'(0 . 2) #'(0 . 20) #0 hello}
  a^\markup { world }
}