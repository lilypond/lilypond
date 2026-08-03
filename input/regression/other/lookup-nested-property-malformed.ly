\version "2.27.3"

\header {
  texidoc = "Attempting to access @code{\\aaa.bbb.err} when @code{aaa} is an
alist, but @code{bbb} is not, triggers an error."
}

expect-error = ##t

aaa.bbb = "B"
x = \aaa.bbb.err
