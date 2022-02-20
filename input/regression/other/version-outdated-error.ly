\header {
  texidoc = "A @code{\\version} statement newer than the running
version of LilyPond should result in an error and a nonzero return
code."
}

%% expect-error checks that there is actually an error.
expect-error = ##t

\version "1000000000.0.0"
