\version "2.25.4"

\header {
  texidoc = "Volte using @code{repeatCommands} can have markup
text."
}

voltaAdLib = \markup { \volta-number { 1. 2. 3... } \italic { ad lib. } }

\relative {
  c''1
  \set Score.repeatCommands = #(list (list 'volta voltaAdLib) 'start-repeat)
  c4 b d e
  \set Score.repeatCommands = #`((volta #f)
                                 (volta ,#{ \markup \volta-number "4." #})
                                 end-repeat)
  f1
  \set Score.repeatCommands = #'((volta #f))
}
