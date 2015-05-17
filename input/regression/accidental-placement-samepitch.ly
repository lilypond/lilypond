\version "2.19.21"

\header {
  texidoc = "When two (or more) accidentals modify the same pitch,
they are printed adjacent to one another unless they represent the same
alteration, in which case they are printed in exactly the same position
as one another. In either case, collisions with accidentals of different
pitches are correctly computed."
}

\relative
{<< <dis'' aeses as a! a! ais aisis a,! aeh cis'> \\ <ais aih> >> }

\paper {ragged-right = ##t}
