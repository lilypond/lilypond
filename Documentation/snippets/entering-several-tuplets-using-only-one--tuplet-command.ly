%% DO NOT EDIT this file manually; it was automatically
%% generated from `Documentation/snippets/new/`.
%%
%% Make any changes in `Documentation/snippets/new/`,
%% then run `scripts/auxiliar/makelsr.pl --new`.
%%
%% This file is in the public domain.
%%
%% Note: this file works from version 2.25.23.

\version "2.25.23"

\header {
  lsrtags = "rhythms, syntax-and-expressions"

  texidoc = "
The property @code{tupletSpannerDuration} sets how long each of the
tuplets contained within the brackets after @code{\\tuplet} should
last. Many consecutive tuplets can then be placed within a single
@code{\\tuplet} expression, thus saving typing.

There are ways to set @code{tupletSpannerDuration} besides using a
@code{\\set} command.  The command @code{\\tupletSpan} sets it to a
given duration, or clears it when instead of a duration
@code{\\default} is specified.  Another way is to use an optional
argument with @code{\\tuplet}.
"

  doctitle = "Entering several tuplets using only one \\tuplet command"
} % begin verbatim


\relative c' {
  \time 2/4
  \tupletSpan 4
  \tuplet 3/2 { c8^"\\tupletSpan 4" c c c c c }
  \tupletSpan \default
  \tuplet 3/2 { c8^"\\tupletSpan \\default" c c c c c }
  \tuplet 3/2 4 { c8^"\\tuplet 3/2 4 {...}" c c c c c }
}
