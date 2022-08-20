\version "2.23.11"

\header {
  lsrtags = "scheme-language, symbols-and-glyphs"

  texidoc = "
If you want to customize the position and/or number of dots in repeat
sign bar lines, you can define new custom bar lines or redefine the way
default repeat signs are drawn.  This snippet shows how.  This may be
particularly helpful when using a staff with custom line-positions, as
shown in this snippet.
"

  doctitle = "Customizing the position and number of dots in repeat sign bar lines"
}


#(define ((make-custom-dot-bar-line dot-positions) grob extent)

   "Draw dots (repeat sign dots) at @var{dot-positions}. The
coordinates of @var{dot-positions} are equivalent to the
coordinates of @code{StaffSymbol.line-positions}, a dot-position
of X and a line-position of X indicate the same vertical position."

   (let* ((staff-space (ly:staff-symbol-staff-space grob))
          (dot (ly:font-get-glyph (ly:grob-default-font grob) "dots.dot"))
          (stencil empty-stencil))
     (for-each
      (lambda (dp)
        (set! stencil (ly:stencil-add stencil
                        (ly:stencil-translate-axis dot (* dp (/ staff-space 2)) Y))))
      dot-positions)
     stencil))

% With the procedure above we can define custom bar-lines, for example,
% one that resembles standard repeat sign bar lines except it has
% three dots at staff positions -3, 0, and 3.

#(add-bar-glyph-print-procedure "*" (make-custom-dot-bar-line '(-3 0 3)))
\defineBarLine ".|*" #'("" "*" "")
\defineBarLine "*|." #'("" "*" "")

% We can also customize the dot positions used in all default repeat signs
% by redefining the print procedure of the colon bar glyph (":"). On a staff
% with line-positions of '(-4 -2 2 4) the default repeat sign dots appear
% at '(-3 3), but we can put them at '(-1 1) instead.

#(add-bar-glyph-print-procedure ":" (make-custom-dot-bar-line '(-1 1)))


\new Staff \with {
  \override StaffSymbol.line-positions = #'(-4 -2 2 4)
  \override StaffSymbol.staff-space = #1.3
} {
  \relative f' {
    g1
    \bar ".|*"
    g
    \bar "*|."
    g
    \bar ".|:-|"
    g
    \bar ":|."
    g
    \repeat volta 2 {
      g
    }
  }
}
