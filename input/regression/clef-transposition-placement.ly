\version "2.19.22"

\header {

  texidoc="Transposition symbols should be correctly positioned
close to the parent clef.  Horizontal alignment is fine-tuned
for standard C, G and F clefs: for example, downwards transposition
of a G clef should be centered exactly under the middle of clef hook.
For clefs that don't have fine-tuned alignment the transposition
number should be centered."

}

% use huge staff-size to see the tiny differencies better.
#(set-global-staff-size 35)

clefVariations =
#(define-music-function (type)(string?)
   #{
     \once \omit Staff.Clef s4
     \override Staff.Clef.full-size-change = ##t
     \clef #(string-append type "8") s4
     \clef #(string-append type "15") s4
     \clef #(string-append type "(8)") s4
     \clef #(string-append type "(141)") s4
     % change clefs are omitted - too similar to regular ones
     \cueClef #(string-append type "8") s4
     \cueClef #(string-append type "15") s4
     \cueClef #(string-append type "(8)") s4
     \cueClef #(string-append type "(141)") s4
   #})

\markup "Even the smallest positioning changes may indicate a problem!"
\score {
  <<
    \new Staff { \clefVariations "C_" }
    \new Staff { \clefVariations "C^" }
    \new Staff { \clefVariations "G_" }
    \new Staff { \clefVariations "G^" }
    \new Staff { \clefVariations "F_" }
    \new Staff { \clefVariations "F^" }
  >>
}

\layout {
  \context {
    \Staff
    \remove Time_signature_engraver
  }
}