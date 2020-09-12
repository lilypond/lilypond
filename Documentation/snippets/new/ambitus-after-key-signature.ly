\version "2.21.5"

\header {

  lsrtags = "pitches,staff-notation,vocal-music"

  texidoc = "
By default, ambitus are positioned at the left of the clef. The
@code{\ambitusAfter} function allows for changing this placement. Syntax is
@code{\ambitusAfter @var{grob-interface}} (see @rinternals{Graphical Object
Interfaces} for a list of possible values for @code{@var{grob-interface}}).
A common use case is printing the ambitus between key signature and time
signature.
"

  doctitle = "Ambitus after key signature"

}

\new Staff \with {
  \consists Ambitus_engraver
} \relative {
  \ambitusAfter key-signature
  \key d \major
  es'8 g bes cis d2
}
