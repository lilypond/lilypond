\version "2.16.0"

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (_ "forced break was overridden by some other event, should you be using bar checks?"))
#(ly:expect-warning (_ "forced break was overridden by some other event, should you be using bar checks?"))

\header {

  texidoc = "If a page break is forced where it is forbidden,
 a warning is printed."

}

\new Staff {
   c'1 \glissando
   \pageBreak
   d'1
}

