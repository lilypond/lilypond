\version "1.7.16"
\header {
texidoc = "Another combination of grace note nesting."
}

\score { \notes \context Voice {

    <
     { \grace  g32 f4 }
    >
    \grace c16 c2. \bar "|."
}
}


%% new-chords-done %%
