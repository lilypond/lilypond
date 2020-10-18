\version "2.23.0"

\header {
  texidoc="@code{\\volta} can add a volta-specific dynamic."
}

music = \context Voice \fixed c' \repeat volta 2 {
  %% TODO: The dynamic engraver complains about conflicting events.
  %%
  %% We could possibly make it render something like "f-p" instead.
  %% We might be able to communicate the volta spec to the dynamic
  %% engraver to help it.
  %%
  %% To hack around this, we could say \unfolded \volta 2 \p and add
  %% some extra markup guarded by \volta #'(), but a more capable
  %% dynamic engraver would be better.
  \volta 1 \f \volta 2 \p a1
}

\score { \music }
\score { \unfoldRepeats \music }
