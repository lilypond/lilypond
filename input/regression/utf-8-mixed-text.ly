\header {
  texidoc = "words in mixed font in a single string
 are separated by spaces as in the input string.
 Here a Russian word followed by a roman word."
  
}

\version "2.16.0"

% Font settings for Cyrillic
% Linux Libertine fonts contain Cyrillic glyphs.
\paper {
  #(define fonts
    (set-global-fonts
     #:roman "Linux Libertine O,serif"
     #:sans "Linux Biolinum O,sans-serif"
     #:typewriter "Linux Libertine Mono O,monospace"
   ))
}

\markup { "Здравствуйте Hallo" }
