
\score { \notes\relative c'' {
\repeat unfold 3 { c^"3x 0a" d }
% less alts than body
\repeat unfold 4 { c^"4x 0a" d } \alternative { e f }
% more alts than body
\repeat unfold 2 { c^"2x 3a" d } \alternative { e f g } 
}}

