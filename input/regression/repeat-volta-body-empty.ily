\version "2.25.34"

\new Staff {
  \repeat volta 2 \testBody | R1
}

\new Staff {
  R1 | \repeat volta 2 \testBody
}

\new Staff {
  R1 | \repeat volta 2 \testBody | R1
}

\new Staff {
  \repeat volta 2 \testBody \alternative { R1_"1." R1_"2." } | R1
}

\new Staff {
  R1 | \repeat volta 2 \testBody \alternative { R1_"1." R1_"2." }
}

\new Staff {
  R1 | \repeat volta 2 \testBody \alternative { R1_"1." R1_"2." } | R1
}
