\header
{

texidoc = "Music identifiers containing arbitrary characters may be
initialized using
@example
\"violin1\" = @{ c''4 c'' c'' c'' @}
@end example
and used as:
@example
\\new Voice @{ \\\"violin1\" @}
@end example
"

}

\version "2.17.28"

"violin1" = { c''4 c'' c'' c'' }
"violin2" = { a'4 a' a' a' }

\layout { ragged-right = ##t }

{
  << \"violin1" \\ \"violin2" >>
}
