\version "1.3.59"

\score { 
  \context Voice \notes\relative c'' {

  \times 2/3 { [c8 c c]  }
  \times 2/3 { c8 [c c]  }

  \property Voice .tupletBracketVisibility = #'if-no-beam  
  \times 2/3 { [c8 c c]  }
  \property Voice .tupletDirection = #1
  \property Voice .tupletNumberVisibility = ##f
  \times 2/3 { c8 [c c]  }
}
}
