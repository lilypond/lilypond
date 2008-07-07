\header {

  texidoc = "If @code{extraNatural} is set then keys that are not
  altered farther away (eg from sharp to double sharp) are
  cancelled. Otherwise only keys that do not occur in the new key
  signature are cancelled."  }


\version "2.11.51"

\paper {
  ragged-right = ##t
}
{
  \set Staff.extraNatural = ##f
  \key fes \major r1
  \key as \major r1_"No B-natural (#f)"
  \set Staff.extraNatural = ##t
  \key gis \major r1
  \key b \major r1_"with F-natural (#t)"

}

