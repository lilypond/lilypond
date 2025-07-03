\version "2.25.27"

\header {
  texidoc = "This tests the internal function tsig-abbr-expand.  Problems are
reported on stderr."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% input is already canonical

#(expect-equal "simple fraction"
  (tsig-abbr-expand '(1 . 2))
  #f)

#(expect-equal "a/b in numerator"
  (tsig-abbr-expand '((1 . 2) . 3))
  #f)

#(expect-equal "a+b in numerator"
  (tsig-abbr-expand '((1 2) . 3))
  #f)

#(expect-equal "a+(b/c) in numerator"
  (tsig-abbr-expand '((1 (2 . 3)) . 4))
  #f)

#(expect-equal "(a/b)+c in numerator"
  (tsig-abbr-expand '(((1 . 2) 3) . 4))
  #f)

#(expect-equal "a+((b+c+d)/e) in numerator"
  (tsig-abbr-expand '((1 ((2 3 4) . 5)) . 6))
  #f)

#(expect-equal "a/b + c/d"
  (tsig-abbr-expand '((1 . 2) (3 . 4)))
  #f)

#(expect-equal "a/b + c/d + e/f"
  (tsig-abbr-expand '((1 . 2) (3 . 4) (5 . 6)))
  #f)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% input is abbreviated

#(expect-equal "simple fraction"
  (tsig-abbr-expand '(1 2))
  '(1 . 2))

#(expect-equal "list of one simple fraction"
  (tsig-abbr-expand '((1 2)))
  '(1 . 2))

#(expect-equal "a/b in numerator"
  (tsig-abbr-expand '((1 2) 3))
  '((1 . 2) . 3))

#(expect-equal "a+b in numerator"
  (tsig-abbr-expand '(1 2 3))
  '((1 2) . 3))

#(expect-equal "a+(b/c) in numerator"
  (tsig-abbr-expand '(1 (2 3) 4))
  '((1 (2 . 3)) . 4))

#(expect-equal "(a/b)+c in numerator"
  (tsig-abbr-expand '((1 2) 3 4))
  '(((1 . 2) 3) . 4))

#(expect-equal "a+((b+c+d)/e) in numerator"
  (tsig-abbr-expand '(1 (2 3 4 5) 6))
  '((1 ((2 3 4) . 5)) . 6))

#(expect-equal "a/b + c/d"
  (tsig-abbr-expand '((1 2) (3 4)))
  '((1 . 2) (3 . 4)))

#(expect-equal "a/b + c/d + e/f"
  (tsig-abbr-expand '((1 2) (3 4) (5 6)))
  '((1 . 2) (3 . 4) (5 . 6)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% input is partly abbreviated

#(expect-equal "a/b in numerator"
  (tsig-abbr-expand '((1 . 2) 3))
  #f)

#(expect-equal "a+(b/c) in numerator"
  (tsig-abbr-expand '(1 (2 . 3) 4))
  #f)

#(expect-equal "(a/b)+c in numerator"
  (tsig-abbr-expand '((1 . 2) 3 4))
  #f)

#(expect-equal "a+((b+c+d)/e) in numerator"
  (tsig-abbr-expand '(1 ((2 3 4) . 5) 6))
  #f)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% other

#(expect-equal "senza misura"
  (tsig-abbr-expand #f)
  #f)
