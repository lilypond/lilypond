\version "2.27.3"

\header {
  texidoc = "Attempting to access @code{\\mod.aaa.bbb.err} where @code{mod} is
a Guile module, @code{aaa} is an alist, and @code{bbb} is neither, triggers
an error."
}

expect-error = ##t

testMod = #(make-module)
#(module-define! testMod 'aaa '((bbb . "B")))
x = \testMod.aaa.bbb.err
