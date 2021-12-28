%%%% Predefined dynamic scripts.
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2005--2022 Sven Axelsson <sven.axelsson@gmail.com>,
%%%%               (FIXME -- date and address?) Andrew McNabb
%%%%
%%%% LilyPond is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%%
%%%% LilyPond is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%%
%%%% You should have received a copy of the GNU General Public License
%%%% along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

%{
  Bagpipe music settings for LilyPond.
  This file builds on work by Andrew McNabb (http://www.mcnabbs.org/andrew/)

  Substantial changes and additions made by
  Sven Axelsson, the Murray Pipes & Drums of Gothenburg
  (http://www.murrays.nu)

  Corrections and additions by Julia Meihoefer and Oliver Briede

  $Id: bagpipe.ly,v 1.12 2006/03/16 14:39:46 hanwen Exp $
%}

\version "2.21.0"

% Notes of the scale of the Great Highland Bagpipe. Extra high notes for bombarde.
% Flat notes used mainly in some modern music.

pitchnamesBagpipe = #`(
  (G . ,(ly:make-pitch 0 4 NATURAL))
  (a . ,(ly:make-pitch 0 5 NATURAL))
  (b . ,(ly:make-pitch 0 6 NATURAL))
  (c . ,(ly:make-pitch 1 0 SHARP))
  (c-flat . ,(ly:make-pitch 1 0 FLAT))
  (cflat . ,(ly:make-pitch 1 0 FLAT))
  (d . ,(ly:make-pitch 1 1 NATURAL))
  (e . ,(ly:make-pitch 1 2 NATURAL))
  (f . ,(ly:make-pitch 1 3 SHARP))
  (f-flat . ,(ly:make-pitch 1 3 FLAT))
  (fflat . ,(ly:make-pitch 1 3 FLAT))
  (g . ,(ly:make-pitch 1 4 NATURAL))
  (g-flat . ,(ly:make-pitch 1 4 FLAT))
  (gflat . ,(ly:make-pitch 1 4 FLAT))
  (A . ,(ly:make-pitch 1 5 NATURAL))
  (B . ,(ly:make-pitch 1 6 NATURAL))
  (C . ,(ly:make-pitch 2 0 SHARP))
)
#(set! language-pitch-names
       (append language-pitch-names
               (list `(bagpipe . ,pitchnamesBagpipe))))
\language "bagpipe"

% Bagpipe music is written in something like D major. If we use
% flattened notes, the flat should be shown on all instances.

hideKeySignature = {
  % We normally don't want to show the key signature.
  \omit Staff.KeySignature
  \set Staff.extraNatural = ##f
  \key d \major
  \accidentalStyle forget
}
showKeySignature = {
  % Show the key signature e.g. for BMW compatibility.
  \override Staff.KeySignature.stencil = #ly:key-signature-interface::print
  \set Staff.extraNatural = ##f
  \key d \major
  \accidentalStyle forget
}

% Layout tweaks.

\layout {
  \context {
    \Voice
    % All stems go down.
    \override Stem.direction = #DOWN
    % All slurs and ties are on top.
    \override Slur.direction = #UP
    \override Tie.direction = #UP
  }
}

% Some common timing tweaks.

% Sets the autobeamer to span quarter notes only. Use for fast music.
quarterBeaming = {
  \set Staff.beamExceptions = #'()
}
halfBeaming = {
  \set Staff.beamExceptions =
  \beamExceptions { 8[ 8 8 8] |
		    \tuplet 3/2 { 8[ 8 8] 8[ 8 8] 8[ 8 8] 8[ 8 8] } }
}

% Reels are in allabreve time with half note beaming.
reelTime = {
  \time 2/2
  \halfBeaming
}
% 4/4 marches are written with numerical time signature and with quarter beaming.
marchTime = {
  \time 4/4
  \numericTimeSignature
  \quarterBeaming
}

% Add appropriate tweaks needed for piping grace notes to look great.
stemspace = #(define-music-function (extent) (pair?) #{
  \once \override Staff.Stem.X-extent = #extent
#})
pgrace = #(define-music-function (notes) (ly:music?) #{
  \override Score.GraceSpacing.spacing-increment = #0
  \override Score.Stem.beamlet-max-length-proportion = #'(0.5 . 0.5)
  \small \grace $notes \normalsize
  \revert Score.Stem.beamlet-default-length
#})

% Single grace notes
grG = { \pgrace { G32 } }
gra = { \pgrace { a32 } }
grb = { \pgrace { b32 } }
grc = { \pgrace { c32 } }
grd = { \pgrace { d32 } }
gre = { \pgrace { e32 } }
grf = { \pgrace { f32 } }
grg = { \pgrace { g32 } }
grA = { \pgrace { A32 } }

% Doublings
dblG = { \pgrace { g32[ G d] } }
dbla = { \pgrace { g32[ a d] } }
dblb = { \pgrace { g32[ b d] } }
dblc = { \pgrace { g32[ c d] } }
dbld = { \pgrace { g32[ d e] } }
dble = { \pgrace { g32[ e f] } }
dblf = { \pgrace { g32[ f g] } }
% These are the same as the half doublings.
dblg = { \pgrace { g32[ f] } }
dblA = { \pgrace { A32[ g] } }

% Half doublings
hdblG = { \pgrace { G32[ d] } }
hdbla = { \pgrace { a32[ d] } }
hdblb = { \pgrace { b32[ d] } }
hdblc = { \pgrace { c32[ d] } }
hdbld = { \pgrace { d32[ e] } }
hdble = { \pgrace { e32[ f] } }
hdblf = { \pgrace { f32[ g] } }
hdblg = { \pgrace { g32[ f] } }
hdblA = { \pgrace { A32[ g] } }

% Thumb doublings
tdblG = { \pgrace { A32[ G d] } }
tdbla = { \pgrace { A32[ a d] } }
tdblb = { \pgrace { A32[ b d] } }
tdblc = { \pgrace { A32[ c d] } }
tdbld = { \pgrace { A32[ d e] } }
tdble = { \pgrace { A32[ e f] } }
tdblf = { \pgrace { A32[ f g] } }
tdblg = { \pgrace { A32[ g f] } }

% Shakes / Pele
% A few of these can't really be played and are here only for consistency.
shakea = { \pgrace { g32[ a e a G] } }
shakeb = { \pgrace { g32[ b e b G] } }
shakec = { \pgrace { g32[ c e c G] } }
shaked = { \pgrace { g32[ d e d G] } }
wshaked = { \pgrace { g32[ d e d c] } }
shakee = { \pgrace { g32[ e f e a] } }
shakef = { \pgrace { g32[ f g f e] } }
shakeg = { \pgrace { A32[ f g a] } }
shakeA = { \pgrace { A32[ g A a] } }

% Half shakes / Half Pele
hshakea = { \pgrace { a32[ e a G] } }
hshakeb = { \pgrace { b32[ e b G] } }
hshakec = { \pgrace { c32[ e c G] } }
hshaked = { \pgrace { d32[ e d G] } }
whshaked = { \pgrace { d32[ e d c] } }
hshakee = { \pgrace { e32[ f e a] } }
hshakef = { \pgrace { f32[ g f e] } }
hshakeg = { \pgrace { g32[ A g f] } }
hshakeA = { \pgrace { A32[ g A a] } }

% Thumb shakes / Thumb Peles
tshakea = { \pgrace { A32[ a e a G] } }
tshakeb = { \pgrace { A32[ b e b G] } }
tshakec = { \pgrace { A32[ c e c G] } }
tshaked = { \pgrace { A32[ d e d G] } }
wtshaked = { \pgrace { A32[ d e d c] } }
tshakee = { \pgrace { A32[ e f e a] } }
tshakef = { \pgrace { A32[ f g f e] } }
tshakeg = { \pgrace { A32[ g A g f] } }
tshakeA = { \pgrace { A32[ g A a] } }

% Slurs / G - Grace Strike
% A few of these can't really be played and are here only for consistency.
slura  = { \pgrace { g32[ a G] } }
slurb  = { \pgrace { g32[ b G] } }
slurc  = { \pgrace { g32[ c G] } }
slurd  = { \pgrace { g32[ d G] } }
wslurd = { \pgrace { g32[ d c] } }
slure  = { \pgrace { g32[ e a] } }
slurf  = { \pgrace { g32[ f e] } }
slurg  = { \pgrace { A32[ f a] } }
slurA  = { \pgrace { f32[ a] } }

% Half slurs / Half Strike
hslura  = { \pgrace { a32[ G] } }
hslurb  = { \pgrace { b32[ G] } }
hslurc  = { \pgrace { c32[ G] } }
hslurd  = { \pgrace { d32[ G] } }
whslurd = { \pgrace { d32[ c] } }
hslure  = { \pgrace { e32[ a] } }
hslurf  = { \pgrace { f32[ e] } }
hslurg  = { \pgrace { g32[ f] } }
hslurA  = { \pgrace { A32[ a] } }

% Thumb slurs / Thumb Strike
tslura  = { \pgrace { A32[ a G] } }
tslurb  = { \pgrace { A32[ b G] } }
tslurc  = { \pgrace { A32[ c G] } }
tslurd  = { \pgrace { A32[ d G] } }
wtslurd = { \pgrace { A32[ d c] } }
tslure  = { \pgrace { A32[ e a] } }
tslurf  = { \pgrace { A32[ f e] } }
tslurg  = { \pgrace { A32[ g f] } }
tslurA  = { \pgrace { f32[ a] } }

% Catches / Half Grip
catcha = { \pgrace { g32[ a G d G] } }
catchb = { \pgrace { g32[ b G d G] } }
catchc = { \pgrace { g32[ c G d G] } }
catchd = { \pgrace { g32[ d G d G] } }
wcatchd = { \pgrace { g32[ d G b G] } }
catche = { \pgrace { g32[ e G d G] } }

% Half catches / G - Grace Grip
hcatcha = { \pgrace { a32[ G d G] } }
hcatchb = { \pgrace { b32[ G d G] } }
hcatchc = { \pgrace { c32[ G d G] } }
hcatchd = { \pgrace { d32[ G d G] } }
whcatchd = { \pgrace { d32[ G b G] } }
hcatche = { \pgrace { e32[ G d G] } }

% Thumb catches
tcatcha = { \pgrace { A32[ a G d G] } }
tcatchb = { \pgrace { A32[ b G d G] } }
tcatchc = { \pgrace { A32[ c G d G] } }
tcatchd = { \pgrace { A32[ d G d G] } }
wtcatchd = { \pgrace { A32[ d G b G] } }
tcatche = { \pgrace { A32[ e G d G] } }

% Triple strikes
% Those that cannnot be played have been omitted.
tripleA = { \pgrace { A32[ g A g A g] } }

% Throws
thrwd     = { \pgrace { G32[ d c] } }
Gthrwd    = { \pgrace { d32[ c] } }
gripthrwd = { \pgrace { G32[ d G c] } }
thrwe     = { \pgrace { e32[ a f a] } }
wthrwe    = { \pgrace { e32[ d f d] } }
thrwf     = { \pgrace { f32[ e g e] } }

% Birls
birl  = { \pgrace { a32[ G a G] } }
wbirl = { \pgrace { G32[ a G] } }
gbirl = { \pgrace { g32[ a G a G] } }
dbirl = { \pgrace { d32[ a G a G] } }

% Grips / Leumluath
grip  = { \pgrace { G32[ d G] } }
bgrip = { \pgrace { G32[ b G] } }
egrip = { \pgrace { G32[ e G] } }

% Taorluaths
taor    = { \pgrace { G32[ d G e] } }
taorjmd = { \pgrace { G32[ d a e] } }
taorold = { \pgrace { G32[ d G a e] } }
btaor   = { \pgrace { G32[ b G e] } }
Gtaor   = { \pgrace { d32[ G e] } }
taoramb = { \pgrace { G32[ d G b e] } }
taoramc = { \pgrace { G32[ d G c e] } }
taoramd = { \pgrace { G32[ d G c d e] } }

% Crunluaths
crun    = { \pgrace { G32[ d G e a f a ] } }
dcrun   = { \pgrace { G32[ b G e a f a ] } }
Gcrun   = { \pgrace { d32[ G e G f a ] } }
crunamb = { \pgrace { G32[ d G b e b f b ] } }
crunamc = { \pgrace { G32[ d G c e c f c ] } }
crunamd = { \pgrace { G32[ d G c d e d f d ] } }
crunambfosg = { \pgrace { e32[ b f b ] } }
crunamcfosg = { \pgrace { e32[ c f c ] } }
crunamdfosg = { \pgrace { e32[ d f d ] } }

% Special piobaireachd notations
grGcad   = { \pgrace { G16 } }
gracad   = { \pgrace { a16 } }
cad      = { \pgrace { \stemspace #'(0 . 0.5) g32[ e8 d32] } }
hcad     = { \pgrace { \stemspace #'(0 . 0.5) g32[ e8] } }
tcad     = { \pgrace { e8[ d32] } }
thcad    = { \pgrace { e8 } }
% This is the same as thrwe
dre      = { \pgrace { e32[ a f a] } }
% This is the same as thrwf
dare     = { \pgrace { f32[ e g e] } }
bari     = { \pgrace { e32[ G f G] } }
dari     = { \pgrace { f32[ e g e f e] } }
pthrwd   = { \pgrace { G16[ d32 c] } }
darodo   = { \pgrace { G32[ d G c G] } }
Gdarodo  = { \pgrace { d32[ G c G] } }
pdarodo  = { \pgrace { G16[ d32 G c G16] } }
pGdarodo = { \pgrace { d32[ G c G16] } }
% Weird stuff from Joseph MacDonald's book
fifteenthcutting     = { \pgrace { G32[ d a e a f a e a d] } }
fifteenthcuttingG    = { \pgrace { G32[ d a e G f G e G d] } }
Gfifteenthcutting    = { \pgrace { d32[ a e a f a e a d] } }
GfifteenthcuttingG   = { \pgrace { d32[ a e G f G e G d] } }
seventeenthcutting   = { \pgrace { G32[ d a e a f a e a d a c] } }
seventeenthcuttingG  = { \pgrace { G32[ d a e G f G e G d G c] } }
Gseventeenthcutting  = { \pgrace { d32[ a e a f a e a d a c] } }
GseventeenthcuttingG = { \pgrace { d32[ a e G f G e G d G c] } }
barluadh   = { \pgrace { G32[ d a e a f a e a d a c a b a e a f a] } }
barluadhG  = { \pgrace { G32[ d a e G f G e G d G c G b G e G f G] } }
Gbarluadh  = { \pgrace { d32[ a e a f a e a d a c a b a e a f a] } }
GbarluadhG = { \pgrace { d32[ a e G f G e G d G c G b G e G f G] } }
% Non-gracenote piobaireachd markup.
trebling = \markup {
  \override #'(baseline-skip . 0.4)
  \column {
    \musicglyph "scripts.tenuto"
    \musicglyph "scripts.tenuto"
    \musicglyph "scripts.tenuto"
  }
}
% Abbreviated notation common in piobaireachd scores.
% TODO: Make sure these are put on a fixed Y-position.
txtaor = \markup { \center-align "T" }
txcrun = \markup { \center-align "C" }
txtaorcrun = \markup {
  \override #'(baseline-skip . 1.8)
  \column {
    \center-align "T"
    \center-align "C"
  }
}
% Turn these upside down, as in the Kilberry book.
txtaoram = \markup { \center-align \scale #'(-1 . -1) "T" }
txcrunam = \markup { \center-align \scale #'(-1 . -1) "C" }
txtaorcrunam = \markup {
  \override #'(baseline-skip . 1.8)
  \column {
    \center-align \scale #'(-1 . -1) "T"
    \center-align \scale #'(-1 . -1) "C"
  }
}
