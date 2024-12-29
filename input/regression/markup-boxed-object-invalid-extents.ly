\version "2.25.23"

\header {
  texidoc = "
In markup, boxing objects with invalid extents works without producing a
warning.
"
}

%% save typing
#(define m-f-b make-filled-box-stencil)
#(define ext-valid '(1 . 3))
#(define ext-invalid '(3 . 1))

\markup box-stil = \markup \box \stencil \etc

valid-Y = \markup \with-color #cyan "Y-ext-valid"
invalid-Y = \markup \with-color #cyan "Y-ext-invalid"
valid-X = \markup \with-color #red "X-ext-valid"
invalid-X = \markup \with-color #red "X-ext-invalid"

\markup {
  \vspace #0.5
  \override #'(box-padding . 1)
  \line {
    \column { \valid-Y
              \line { \box-stil #(m-f-b ext-valid ext-valid)
              \valid-X } }
    \column { \invalid-Y
              \line { \box-stil #(m-f-b ext-valid ext-invalid)
              \valid-X } }
    \column { \valid-Y
              \line { \box-stil #(m-f-b ext-invalid ext-valid)
              \invalid-X } }
    \column { \invalid-Y
              \line { \box-stil #(m-f-b ext-invalid ext-invalid)
              \invalid-X } }
  }
}
