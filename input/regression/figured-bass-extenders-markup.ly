\version "2.16.0"

\header {

  texidoc = "When using extender lines in FiguredBass, markup objects should
be treated like ordinary figures and work correctly with extender lines.

Extenders should only be used if the markup is really identical."

}


\paper { ragged-right = ##t }

<<
  \context Voice {
    c'4 c' c' c'
    c' c' c' c'
  }
  \figures {
    \bassFigureExtendersOn
    <6>4 <\markup{"Text"}> s <\markup{\draw-circle #0.7 #0.1 ##f }> |
    <\markup{\draw-circle #0.7 #0.1 ##f } 5>
      <\markup{\draw-circle #0.5 #0.1 ##t }>
      <\markup{\draw-circle #0.7 #0.1 ##t }> <6>
  }
>>
