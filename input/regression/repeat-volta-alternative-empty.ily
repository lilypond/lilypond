\version "2.25.34"

\new Staff {
  \repeat volta 2 {
    R1_"body only"
    \alternative {
      \volta 1 \testMusic
      \volta 2 \testMusic
    }
  }
}

\new Staff {
  \repeat volta 2 {
    R1
    \alternative {
      \volta 1 R1_"1."
      \volta 2 \testMusic
    }
  }
}

\new Staff {
  \repeat volta 2 {
    R1
    \alternative {
      \volta 1 \testMusic
      \volta 2 R1_"2."
    }
  }
}

\new Staff {
  \repeat volta 3 {
    R1
    \alternative {
      \volta 1 R1_"1."
      \volta 2 \testMusic
      \volta 3 R1_"3."
    }
  }
}

\new Staff {
  \repeat volta 5 {
    R1
    \alternative {
      \volta 1   R1_"1."
      \volta 2,3 \testMusic
      \volta 4,5 R1_"4.5."
    }
  }
}
