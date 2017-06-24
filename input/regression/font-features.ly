\version "2.19.60"

\header
{
  texidoc = "Exercise font features. Requires a font that supports the
    features. This ensures no errors using the interface."
}

% Comparison between caps styles
\markup { Hello }
\markup { HELLO }
\markup { \caps Hello }
\markup { \fontCaps Hello }
% True small caps
\markup { \override #'(font-features . ("smcp")) Hello }

% Comparison between number styles
\markup { 0123456789 }
\markup { \override #'(font-features . ("onum")) 0123456789 }

% Multiple features
\markup { \override #'(font-features . ("onum" "smcp")) { Hello 0123456789 } }
