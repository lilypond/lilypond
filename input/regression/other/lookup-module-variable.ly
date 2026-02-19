\version "2.25.34"

\header {
  texidoc = "Scoped variable references beginning with the name of a module
perform module lookups until a non-module is found, then drop into alist
lookups.  Problems are reported in the standard error stream."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)

testMod = #(make-module)
#(module-define! testMod 'ant 0)
#(module-define! testMod 'bee '((1 . a) (2 . b)))
#(module-define! testMod 'cat '((A . 3) (B . 4)))
#(module-define! testMod 'mod (make-module))
#(module-define! (module-ref testMod 'mod) 'dog 5)
#(module-define! (module-ref testMod 'mod) 'emu '((6 . c) (7 . d)))
#(module-define! (module-ref testMod 'mod) 'fox '((C . 8) (D . 9)))
%% alist value is a module
#(module-define! testMod 'weird `((indeed . ,(module-ref testMod 'mod))))

#(expect-equal "" #{ \testMod #} testMod)
#(expect-equal "" #{ \testMod.ant #} 0)
#(expect-equal "" #{ \testMod.bee #} '((1 . a) (2 . b)))
#(expect-equal "" #{ \testMod.bee.1 #} 'a)
#(expect-equal "" #{ \testMod.bee.2 #} 'b)
#(expect-equal "" #{ \testMod.cat #} '((A . 3) (B . 4)))
#(expect-equal "" #{ \testMod.cat.A #} 3)
#(expect-equal "" #{ \testMod.cat.B #} 4)
#(expect-equal "" #{ \testMod.mod.dog #} 5)
#(expect-equal "" #{ \testMod.mod.emu #} '((6 . c) (7 . d)))
#(expect-equal "" #{ \testMod.mod.emu.6 #} 'c)
#(expect-equal "" #{ \testMod.mod.emu.7 #} 'd)
#(expect-equal "" #{ \testMod.mod.fox #} '((C . 8) (D . 9)))
#(expect-equal "" #{ \testMod.mod.fox.C #} 8)
#(expect-equal "" #{ \testMod.mod.fox.D #} 9)

#(expect-equal
  "lookup finds a module"
  #{ \testMod.mod #}
  (module-ref testMod 'mod))

#(expect-equal
  "lookup ends at a module in an alist"
  #{ \testMod.weird.indeed #}
  #{ \testMod.mod #})

%% TODO: Can we test that this produces a useful diagnostic?
%% We don't search modules found in alists.
%% x = \testMod.weird.indeed.fox

%% The aim of this file is to verify access to Scheme modules generally, but an
%% important special case is reading header variables.
myHeader = \header { title = "lalala" }
#(expect-equal "" #{ \myHeader.title #} "lalala")
