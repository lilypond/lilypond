\version "1.3.110"

\score { 
  \context Voice \notes\relative c'' {

	\times 2/3 { c'8 c,, c }
	\times 2/3 { c'8 c'' c,, }

  
  \times 2/3 { [c8 c c]  }
  \times 2/3 { c8 [c c]  }

    \times 2/3 { [c8 c c]  }
  \times 2/4 { r8 [c, c'] r8 }


  
  \property Voice.TupletBracket \set #'tuplet-bracket-visibility = #'if-no-beam  
  \times 2/3 { [c8 c c]  }
  \property Voice.TupletBracket \set #'direction = #1
  \property Voice.TupletBracket \set #'tuplet-number-visibility = ##f
  \times 2/3 { c8 [c c]  }
    

}
}
