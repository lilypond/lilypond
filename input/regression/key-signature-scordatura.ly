
\header { texidoc = "Key signatures can be set per pitch
individually. This can be done bby setting @code{Staff.keySignature}
directly.";

}
\score { \notes
\relative c'
\context Staff {
  \property Staff.keySignature = #'(((1 .  2) . 1) ((0 . 3) . -1))
  f8 a c e
  \property Staff.keySignature = #'(((1 .  2) . -1) ((0 . 4) . 2))
  e a, g a
}
}
