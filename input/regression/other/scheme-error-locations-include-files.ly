\version "2.24.2"

\header {
  texidoc = "In @code{-dcompile-scheme-code} mode, source locations
are printed for Scheme errors, even if the faulty Scheme code
is in an include file."
}

expect-error = ##t

#(ly:set-option 'compile-scheme-code)

\include "scheme-error-locations-code.ily"

#(f 'oops)
