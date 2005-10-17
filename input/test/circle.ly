
\version "2.7.13"
\header {
texidoc = "@cindex Circle

Circles can be drawn around various objects.

"
}

\layout{raggedright = ##t}

\relative c'{
c1
\set Score.markFormatter
  = #(lambda (mark context)
             (make-circle-markup (format-mark-numbers mark context)))
\mark \default
c2 d^\markup{\circle \finger "2"}
\override Score.BarNumber #'break-visibility = #all-visible
\override Score.BarNumber #'callbacks #'stencil
  = #(make-stencil-circler 0.1 0.25 Text_interface::print)
}