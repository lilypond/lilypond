\version "2.19.21"

\header {
  texidoc = "Scripts avoid ties.
"
}

% Managing what note heads' scripts have been accounted for in
% the Tie_engraver is tricky.
% This regtest tries to confuse it with various configurations
% Of notes with and without ties, with multiple scripts, etc..

\relative {
  r2. c'''4~-> | c-> r2. |
  r2. c4-> | c-> r2. |
  r2. c4~-> | c r2. |
  r2. c4~ | 4-> r2. | \break
  r2. <g-- c-> >4--~ | <g-- c>-> ~ <g c---_>-> r2 |
  r2. c4~ | 4-> ~ c-> r2 |
  r2. c4~-> | c ~ c-> r2 |
  r2. c4~-> | c-> ~ c r2 |
  r2. c4-> |
}
