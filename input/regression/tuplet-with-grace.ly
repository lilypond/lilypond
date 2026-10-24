\version "2.27.5"

\header { texidoc = "When grace notes occur inside of a
@code{\\tuplet} command, they are only covered by tuplet brackets or
beams when no tuplet duration span is active for splitting the command
into groups." }

\layout { ragged-right = ##t }

\relative c' {
  <>^"Automatically split"
  \tuplet 3/2 4 { \grace b8 c8 d e e f g \grace a g f e \grace f e d c } |
  <>^"Not automatically split"
  \tuplet 3/2 { \grace b8 c8 d e } c2.
}
