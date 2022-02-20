\version "2.23.7"

\header {
  texidoc = "An invalid @code{\\version} statement should result in an error
and a nonzero return code."
}

%% expect-error checks that there is actually an error.
expect-error = ##t

\version "foobar"
