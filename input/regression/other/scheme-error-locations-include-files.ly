\version "2.23.14"

\header {
  texidoc = "In @code{-dcompile-scheme-code} mode, source locations
are printed for Scheme errors, even if the faulty Scheme code
is in an include file."
}

#(ly:set-option 'compile-scheme-code)

\include "scheme-error-locations-code.ily"

#(f 'oops)
