%{
  Bagpipe music settings for LilyPond.
  This file builds on work by Andrew McNabb (http://www.mcnabbs.org/andrew/)
  
  Substantial changes and additions made by 
  Sven Axelsson, the Murray Pipes & Drums of Gothenburg
  (http://www.murrays.nu)
  
  $Id: bagpipe.ly,v 1.12 2006/03/16 14:39:46 hanwen Exp $
%}

\version "2.12.0"

% Notes of the scale of the Great Highland Bagpipe. Extra high notes for bombarde.
% Flat notes used mainly in some modern music.

pitchnamesBagpipe = #`(
  (G . ,(ly:make-pitch 0 4 NATURAL))
  (a . ,(ly:make-pitch 0 5 NATURAL))
  (b . ,(ly:make-pitch 0 6 NATURAL))
  (c . ,(ly:make-pitch 1 0 SHARP))
  (cflat . ,(ly:make-pitch 1 0 FLAT))
  (d . ,(ly:make-pitch 1 1 NATURAL))
  (e . ,(ly:make-pitch 1 2 NATURAL))
  (f . ,(ly:make-pitch 1 3 SHARP))
  (fflat . ,(ly:make-pitch 1 3 FLAT))
  (g . ,(ly:make-pitch 1 4 NATURAL))
  (gflat . ,(ly:make-pitch 1 4 FLAT))
  (A . ,(ly:make-pitch 1 5 NATURAL))
  (B . ,(ly:make-pitch 1 6 NATURAL))
  (C . ,(ly:make-pitch 2 0 SHARP))
)
pitchnames = \pitchnamesBagpipe
#(ly:parser-set-note-names parser pitchnames)

% Bagpipe music is written in something like D major. If we use
% flattened notes, the flat should be shown on all instances.

hideKeySignature = {
  % We normally don't want to show the key signature.
  \override Staff.KeySignature  #'stencil = ##f
  \set Staff.extraNatural = ##f
  \key d \major
  #(set-accidental-style 'forget)
}
showKeySignature = {
  % Show the key signature e.g. for BMW compatibility.
  \override Staff.KeySignature  #'stencil = #ly:key-signature-interface::print
  \set Staff.extraNatural = ##f
  \key d \major
  #(set-accidental-style 'forget)
}

% Layout tweaks.

\layout {
  \context {
    \Voice
    % All stems go down.
    \override Stem #'direction = #DOWN
    % All slurs and ties are on top.
    \override Slur #'direction = #UP
    \override Tie #'direction = #UP
  }
}

% Some common timing tweaks.

% Sets the autobeamer to span quarter notes only. Use for fast music.
% TODO: Needs more tweaking
quarterBeaming = {
  #(override-auto-beam-setting '(end * * * *) 1 4 'Staff)
  #(override-auto-beam-setting '(end * * * *) 1 2 'Staff)
  #(override-auto-beam-setting '(end * * * *) 3 4 'Staff)
  #(override-auto-beam-setting '(end * * * *) 4 4 'Staff)
  #(revert-auto-beam-setting '(end 1 32 2 4 ) 1 8 'Staff)
  #(revert-auto-beam-setting '(end 1 32 2 4 ) 3 8 'Staff)
  #(revert-auto-beam-setting '(end 1 32 4 4 ) 1 8 'Staff)
  #(revert-auto-beam-setting '(end 1 32 4 4 ) 3 8 'Staff)
  #(revert-auto-beam-setting '(end 1 32 4 4 ) 5 8 'Staff)
  #(revert-auto-beam-setting '(end 1 32 4 4 ) 7 8 'Staff)
}
halfBeaming = {
  #(override-auto-beam-setting '(end * * 2 2) 1 2 'Staff)
  #(override-auto-beam-setting '(end * * 2 2) 2 2 'Staff)
}
% Reels are in allabreve time with half note beaming.
reelTime = {
  \time 2/2
  \halfBeaming
}
% 4/4 marches are written with numerical time signature and with quarter beaming.
marchTime = {
  \time 4/4
  \override Staff.TimeSignature #'style = #'()
  \quarterBeaming
}

% Single grace notes
grG = { \grace { \small G32 } }
gra = { \grace { \small a32 } }
grb = { \grace { \small b32 } }
grc = { \grace { \small c32 } }
grd = { \grace { \small d32 } }
gre = { \grace { \small e32 } }
grf = { \grace { \small f32 } }
grg = { \grace { \small g32 } }
grA = { \grace { \small A32 } }

% Doublings
dblG = { \grace { \small g32[ G d] } }
dbla = { \grace { \small g32[ a d] } }
dblb = { \grace { \small g32[ b d] } }
dblc = { \grace { \small g32[ c d] } }
dbld = { \grace { \small g32[ d e] } }
dble = { \grace { \small g32[ e f] } }
dblf = { \grace { \small g32[ f g] } }
% These are the same as the half doublings.
dblg = { \grace { \small g32[ f] } }
dblA = { \grace { \small A32[ g] } }

% Half doublings
hdblG = { \grace { \small G32[ d] } }
hdbla = { \grace { \small a32[ d] } }
hdblb = { \grace { \small b32[ d] } }
hdblc = { \grace { \small c32[ d] } }
hdbld = { \grace { \small d32[ e] } }
hdble = { \grace { \small e32[ f] } }
hdblf = { \grace { \small f32[ g] } }
hdblg = { \grace { \small g32[ f] } }
hdblA = { \grace { \small A32[ g] } }

% Thumb doublings
tdblG = { \grace { \small A32[ G d] } }
tdbla = { \grace { \small A32[ a d] } }
tdblb = { \grace { \small A32[ b d] } }
tdblc = { \grace { \small A32[ c d] } }
tdbld = { \grace { \small A32[ d e] } }
tdble = { \grace { \small A32[ e f] } }
tdblf = { \grace { \small A32[ f g] } }
tdblg = { \grace { \small A32[ g f] } }

% Shakes
% A few of these can't really be played and are here only for consistency.
shakea = { \grace { \small g32[ a d a G] } }
shakeb = { \grace { \small g32[ b d b G] } }
shakec = { \grace { \small g32[ c d c G] } }
shaked = { \grace { \small g32[ d e d G] } }
shakee = { \grace { \small g32[ e f e a] } }
shakef = { \grace { \small g32[ f g f a] } }
shakeg = { \grace { \small A32[ f g a] } }
shakeA = { \grace { \small A32[ g A a] } }

% Half shakes
hshakea = { \grace { \small a32[ d a G] } }
hshakeb = { \grace { \small b32[ d b G] } }
hshakec = { \grace { \small c32[ d c G] } }
hshaked = { \grace { \small d32[ e d G] } }
hshakee = { \grace { \small e32[ f e a] } }
hshakef = { \grace { \small f32[ g f a] } }
hshakeg = { \grace { \small g32[ f g a] } }
hshakeA = { \grace { \small A32[ g A a] } }

% Thumb shakes
tshakea = { \grace { \small A32[ a d a G] } }
tshakeb = { \grace { \small A32[ b d b G] } }
tshakec = { \grace { \small A32[ c d c G] } }
tshaked = { \grace { \small A32[ d e d G] } }
tshakee = { \grace { \small A32[ e f e a] } }
tshakef = { \grace { \small A32[ f g f a] } }
tshakeg = { \grace { \small A32[ f g a] } }
tshakeA = { \grace { \small A32[ g A a] } }

% Slurs
% A few of these can't really be played and are here only for consistency.
slura = { \grace { \small g32[ a G] } }
slurb = { \grace { \small g32[ b G] } }
slurc = { \grace { \small g32[ c G] } }
slurd = { \grace { \small g32[ d G] } }
slure = { \grace { \small g32[ e a] } }
slurf = { \grace { \small g32[ f a] } }
slurg = { \grace { \small A32[ f a] } }
slurA = { \grace { \small f32[ a] } }

% Half slurs
hslura = { \grace { \small a32[ G] } }
hslurb = { \grace { \small b32[ G] } }
hslurc = { \grace { \small c32[ G] } }
hslurd = { \grace { \small d32[ G] } }
hslure = { \grace { \small e32[ a] } }
hslurf = { \grace { \small f32[ a] } }
hslurg = { \grace { \small g32[ a] } }
hslurA = { \grace { \small A32[ a] } }

% Thumb slurs
tslura = { \grace { \small A32[ a G] } }
tslurb = { \grace { \small A32[ b G] } }
tslurc = { \grace { \small A32[ c G] } }
tslurd = { \grace { \small A32[ d a] } }
tslure = { \grace { \small A32[ e a] } }
tslurf = { \grace { \small A32[ f a] } }
tslurg = { \grace { \small A32[ f a] } }
tslurA = { \grace { \small f32[ a] } }

% Catches
catcha = { \grace { \small a32[ G d G] } }
catchb = { \grace { \small b32[ G d G] } }
catchc = { \grace { \small c32[ G d G] } }
catchd = { \grace { \small d32[ G b G] } }
catche = { \grace { \small e32[ G d G] } }

% G-grace catches
gcatcha = { \grace { \small g32[ a G d G] } }
gcatchb = { \grace { \small g32[ b G d G] } }
gcatchc = { \grace { \small g32[ c G d G] } }
gcatchd = { \grace { \small g32[ d G b G] } }
gcatche = { \grace { \small g32[ e G d G] } }

% Thumb catches
tcatcha = { \grace { \small A32[ a G d G] } }
tcatchb = { \grace { \small A32[ b G d G] } }
tcatchc = { \grace { \small A32[ c G d G] } }
tcatchd = { \grace { \small A32[ d G b G] } }
tcatche = { \grace { \small A32[ e G d G] } }

% Throws
thrwd     = { \grace { \small G32[ d c] } }
Gthrwd    = { \grace { \small d32[ c] } }
gripthrwd = { \grace { \small G32[ d G c] } }
thrwf     = { \grace { \small f32[ e g e] } }

% Birls
birl  = { \grace { \small a32[ G a G] } }
wbirl = { \grace { \small G32[ a G] } }
gbirl = { \grace { \small g32[ a G a G] } }
dbirl = { \grace { \small d32[ a G a G] } }

% Grips
grip  = { \grace { \small G32[ d G] } }
dgrip = { \grace { \small G32[ b G] } }
egrip = { \grace { \small G32[ e G] } }

% Taorluaths
taor    = { \grace { \small G32[ d G e] } }
dtaor   = { \grace { \small G32[ b G e] } }
Gtaor   = { \grace { \small d32[ G e] } }
taoramb = { \grace { \small G32[ d G b e] } }
taoramc = { \grace { \small G32[ d G c e] } }
taoramd = { \grace { \small G32[ d G c d e] } }

% Crunluaths
crun    = { \grace { \small G32[ d G e a f a ] } }
dcrun   = { \grace { \small G32[ b G e a f a ] } }
Gcrun   = { \grace { \small d32[ G e G f a ] } }
crunamb = { \grace { \small G32[ d G b e b f b ] } }
crunamc = { \grace { \small G32[ d G c e c f c ] } }
crunamd = { \grace { \small G32[ d G c d e d f d ] } }

% Special piobaireachd notations
grGcad  = { \grace { \small G16 } }
gracad  = { \grace { \small a16 } }
cad     = { \grace { \small g32[ e8 d32] } }
hcad    = { \grace { \small g32[ e8] } }
dre     = { \grace { \small e32[ a f a] } }
% This is the same as thrwf
dare    = { \grace { \small f32[ e g e] } }
bari    = { \grace { \small e32[ G f G] } }
dari    = { \grace { \small f32[ e g e f e] } }
pthrwd  = { \grace { \small G16[ d32 c] } }
darodo  = { \grace { \small G32[ d G c G] } }
Gdarodo = { \grace { \small d32[ G c G] } }
% Non-gracenote piobaireachd markup.
trebling = \markup { 
  \override #'(baseline-skip . 0.3)
  \column { 
    \musicglyph #"scripts.tenuto" 
    \musicglyph #"scripts.tenuto" 
    \musicglyph #"scripts.tenuto" 
  }
}
% Abbreviated notation common in piobaireachd scores.
% TODO: Make sure these are put on a fixed Y-position.
txtaor = \markup { "T" }
txcrun = \markup { "C" }
txtaorcrun = \markup { \column { "T" "C" } }
% TODO: These characters should be shown upside down.
% Use a postscript markup command for this.
txtaoram = \markup { "T" }
txcrunam = \markup { "C" }
txtaorcrunam = \markup { \column { "T" "C" } }
