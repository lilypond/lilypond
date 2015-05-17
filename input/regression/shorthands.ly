\version "2.19.21"

\header {

texidoc = "A number of shorthands like @code{(}, @code{)}, @code{|},
@code{[}, @code{]}, @code{~}, @code{\\(}, @code{\\)} and others can be
redefined like normal commands.  @file{ly/declarations-init.ly} serves
as a regtest for a number of them.  This test just demonstrates
replacing @code{(} and @code{)} with melismata commands which are
@emph{not} articulations."

}

\layout { ragged-right = ##t }

"\\{" = (
"\\}" = )
"(" = \melisma
")" = \melismaEnd

\new Staff <<
  \relative {
    c'8 \{ d e f \} % slurred
    g ( a b c ) % no slur, but with melisma
    c,1 \bar "|."
   }
   \addlyrics { Li -- ly -- pond. }
>>
