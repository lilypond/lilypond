% script-init.ly
%%%% Scripts.
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

\version "2.21.0"

harmonic = #(make-music 'HarmonicEvent)

accent = #(make-articulation 'accent
           'midi-extra-velocity 20)
coda = #(make-articulation 'coda)
downbow = #(make-articulation 'downbow)
downmordent = #(make-articulation 'downmordent)
downprall = #(make-articulation 'downprall)
espressivo = #(make-articulation 'espressivo)
fermata = #(make-articulation 'fermata)
flageolet = #(make-articulation 'flageolet)
halfopen = #(make-articulation 'halfopen)
haydnturn = #(make-articulation 'haydnturn)
henzelongfermata = #(make-articulation 'henzelongfermata)
henzeshortfermata = #(make-articulation 'henzeshortfermata)
lheel = #(make-articulation 'lheel)
lineprall = #(make-articulation 'lineprall)
longfermata = #(make-articulation 'longfermata)
ltoe = #(make-articulation 'ltoe)
marcato = #(make-articulation 'marcato
            'midi-extra-velocity 40)
mordent = #(make-articulation 'mordent)
open = #(make-articulation 'open)

portato = #(make-articulation 'portato
            'midi-length
            (lambda (len context)
             (ly:moment-mul len (ly:make-moment 3/4))))
prall = #(make-articulation 'prall)
pralldown = #(make-articulation 'pralldown)
prallmordent = #(make-articulation 'prallmordent)
prallprall = #(make-articulation 'prallprall)
prallup = #(make-articulation 'prallup)
reverseturn = #(make-articulation 'reverseturn)
rheel = #(make-articulation 'rheel)
rtoe = #(make-articulation 'rtoe)
segno = #(make-articulation 'segno)
shortfermata = #(make-articulation 'shortfermata)
signumcongruentiae = #(make-articulation 'signumcongruentiae)
slashturn = #(make-articulation 'slashturn)
snappizzicato = #(make-articulation 'snappizzicato)
staccatissimo = #(make-articulation 'staccatissimo
                  'midi-length
                  (lambda (len context)
                    (seconds->moment 1/8 context))
                  'midi-extra-velocity 6)
staccato = #(make-articulation 'staccato
             'midi-length
             (lambda (len context)
               (moment-min (ly:moment-mul len (ly:make-moment 1/2))
                           (seconds->moment 1/2 context)))
             'midi-extra-velocity 4)
stopped = #(make-articulation 'stopped)
tenuto = #(make-articulation 'tenuto)
thumb = \finger \markup \scale #(cons (magstep 5) (magstep 5))
                        \musicglyph "scripts.thumb"
trill = #(make-articulation 'trill)
turn = #(make-articulation 'turn)
upbow = #(make-articulation 'upbow)
upmordent = #(make-articulation 'upmordent)
upprall = #(make-articulation 'upprall)
varcoda = #(make-articulation 'varcoda)
verylongfermata = #(make-articulation 'verylongfermata)
veryshortfermata = #(make-articulation 'veryshortfermata)

% code char abbreviations
dashHat = \marcato
dashPlus = \stopped
dashDash = \tenuto
dashBang = \staccatissimo
dashLarger = \accent
dashDot = \staccato
dashUnderscore = \portato
