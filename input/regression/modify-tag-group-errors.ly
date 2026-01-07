\version "2.25.33"

\header {
  texidoc = "Test tag group modification errors."
}

expect-error = ##t
#(ly:set-option 'warning-as-error #t)

tgI = \tagGroupRef foo,bar,baz

#(ly:expect-warning "conflicting tag group (foo bar baz)")
\tagGroup foo,test

#(ly:expect-warning "tag group (foo test) not found")
\resetTagGroup foo,test

#(ly:expect-warning "tag group (foo bar) not found")
\addToTagGroup foo,bar bar

\addToTagGroup foo,bar,baz test

#(ly:expect-warning "tag group (foo bar baz) not found")
\removeFromTagGroup \tgI test

\removeFromTagGroup foo,bar,baz,test test

\addToTagGroup \tgI test

tgII = \tagGroupRef x,y,z

#(ly:expect-warning "conflicting tag group (x y z)")
\addToTagGroup \tgI x

\resetTagGroups

tgI = \tagGroupRef foo,bar,baz
\addToTagGroup \tgI \tgII
