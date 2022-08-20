\version "2.23.6"

\header {
  lsrtags = "expressive-marks"

  texidoc = "
The shorthands are defined in @samp{ly/script-init.ly}, where the
variables @code{dashHat}, @code{dashPlus}, @code{dashDash},
@code{dashBang}, @code{dashLarger}, @code{dashDot}, and
@code{dashUnderscore} are assigned default values.  The default values
for the shorthands can be modified. For example, to associate the
@code{-+} (@code{dashPlus}) shorthand with the @emph{trill} symbol
instead of the default @emph{+} symbol, assign the value @code{\\trill}
to the variable @code{dashPlus}:
"

  doctitle = "Modifying default values for articulation shorthand notation"
}


\relative c'' { c1-+ }

dashPlus = \trill

\relative c'' { c1-+ }
