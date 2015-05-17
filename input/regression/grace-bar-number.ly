
\version "2.19.21"
\header {

  texidoc = "Grace notes do tricky things with timing. If a measure
starts with a grace note, the measure does not start at 0, but
earlier. Nevertheless, lily should not get confused. For example, line
breaks should be possible at grace notes, and the bar number should be
printed correctly.
"
}
\layout { ragged-right = ##t }


\relative { c''1 \break
		\grace c8
		c1  }




