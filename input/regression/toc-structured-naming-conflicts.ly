\version "2.23.11"

\header {
  texidoc = "In structured tables of contents, the first
path component of an entry can refer to a previously defined
node anywhere in the tree.  The rest of the path is directly
interpreted from this initial node."
}

\markuplist \table-of-contents

\tocItem foo "Foo"
\tocItem foo.fooII "Foo 2"
\tocItem foo.fooII.bar "Bar"
\tocItem foo.fooII.bar.baz "Baz"

\tocItem spam "Spam"
%% This shouldn't cause a conflict with foo.fooII.bar
\tocItem spam.bar "Spam bar"
\tocItem spam.bar.eggs "Spam bar eggs"
%% This should be attached to the last 'bar' node that was
%% seen, the one from spam.
\tocItem bar.barbar "Spam bar barbar"

{ c' }
