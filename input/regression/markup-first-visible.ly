\version "2.21.0"

\header {
  texidoc = "The markup command @code{\\first-visible} uses the first argument that produces a non-empty stencil and ignores the rest.

The expected markup on this score is \"Lame Songs for Testing\" followed by a \"C\" time signature symbol."

  publication = "Lame Songs for Testing"
}

#(ly:expect-warning "Cannot find glyph nonesuch-1")
#(ly:expect-warning "Cannot find glyph nonesuch-2")

\paper {
  scoreTitleMarkup = \markup {
    \first-visible {
      \musicglyph "nonesuch-1"
      \fromproperty #'header:composer
      \italic \fromproperty #'header:publication
    }
    \first-visible {
      \musicglyph "timesig.C44"
      \musicglyph "this should not be attempted"
    }
  }
}

\score { f' }

\markup {
  No elements: \first-visible {}
}

\markup {
  One element (expect 111): \first-visible { 111 }
}

\markup {
  Single markup list (expect aaa):
  \first-visible \column-lines {
  \musicglyph "nonesuch-2"
  ""
  aaa
  bbb
  }
}

\markup {
  Multiple markup lists (expect ccc):
  \first-visible {
    \column-lines { }
    \column-lines { ccc ddd }
    \column-lines { eee }
  }
}

\markup {
  Mixed markup and markup lists (expect fff):
  \first-visible {
    ""
    \column-lines { }
    \normal-text fff
    \column-lines { ggg hhh }
    iii
  }
}

\markup {
  Nested markup lists (expect jjj):
  \first-visible {
    \column-lines {
      \column-lines {
        \column-lines {
	  ""
        }
      }
    }
    \column-lines {
      \column-lines {
        \column-lines {
          jjj
        }
      }
    }
    kkk
  }
}
