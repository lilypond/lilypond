
%  no alts.
\score { \notes
  \relative c'' {
\repeat volta 3 { c^"3$\\times$ 0alt" d }
% less alts than body
\repeat volta 4 { c^"4$\\times$ 2alt" d } \alternative { e f }

% more alts than body
\repeat volta 2 { c^"2$\\times$ 3alt" d } \alternative { e f g } 
}}

