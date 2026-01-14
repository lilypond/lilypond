\version "2.25.33"

\header {
  texidoc = "The extreme high and low notes should have triangular heads
centered on the stems, pointing away from the flag end.  They should not have
ledger lines; however, normal note heads that share a stem with them should have
ledger lines.

Normal note heads sharing stems with arrow heads @emph{should} be aligned
normally; it is a known issue that LilyPond centers them too."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  ragged-right = ##t
}

{
  \fixed c'' { % above staff
    <c \approximatePitch c'>4.
    <a \approximatePitch f'>8
    \approximatePitch c'2
  }

  \fixed c { % below staff
    \approximatePitch a2
    <\approximatePitch a c''>4.
    <\approximatePitch d c'>8
  }
}
