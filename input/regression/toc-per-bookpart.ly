\version "2.27.5"

\header {
  texidoc = "Bookparts or other subunits can have separate
tables of contents as long as they have separate paper blocks."
  tagline = ##f
}

\layout { ragged-right = ##t ragged-bottom = ##t }

\book {
  \bookpart {
    \paper { }
    %% a separate paper block is needed for a separate table of contents
    \markuplist \table-of-contents
    \tocItem \markup "C major"
    \score { <c' e' g'>1 }
    \tocItem \markup "C minor"
    \score { <c' es' g'> }
  }

  \bookpart {
    \paper { }
    \markuplist \table-of-contents
    \tocItem \markup "G major"
    \score { <g b d'> }
    \tocItem \markup "G minor"
    \score { <g bes d'> }
  }
}
