\version "2.27.5"

\header { texidoc = "The autobeamer should complete its work even when
a context end is cut short by a grace note, like it happens with
temporary polyphony." }

\layout { ragged-right = ##t }

{
  \time 2/4
  b'8 a' \grace a'16 g'4
  %% These are beamed normally:
  << {b'8 a'} \\ {c'4} >> g'4
  %% These _should_ be beamed:
  << {b'8 a'} \\ {c'4} >> \grace a'16 g'4
  %% Moving the grace note in the next bar:
  r4 << {b'8 a'} \\ {c'4} >> \grace a'16 g'2
}
