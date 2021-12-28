%%%% Turkish makam note names.
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2019--2022 Adam Good <goodadamgood@gmail.com>
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

\version "2.23.3"

%{

Define makam alterations 72 ET

%}

#(define-public KOMA 1/12) % KOMA 1/9
#(define-public CEYREK 3/12) % CEYREK quarter 1/4
#(define-public EKSIK-BAKIYE 4/12) % EKSIK-BAKIYE 3/9
#(define-public BAKIYE 5/12) % BAKIYE 4/9
#(define-public KUCUK 6/12) % KUCUK 5/9
#(define-public BUYUKMUCENNEB 10/12) % BUYUKMUCENNEB 8/9
#(define-public TANINI 11/12) % 10/9

%{

Define pitch names

%}

turkishMakamPitchNames = #`(
  (c . ,(ly:make-pitch -1 0 NATURAL))
  (d . ,(ly:make-pitch -1 1 NATURAL))
  (e . ,(ly:make-pitch -1 2 NATURAL))
  (f . ,(ly:make-pitch -1 3 NATURAL))
  (g . ,(ly:make-pitch -1 4 NATURAL))
  (a . ,(ly:make-pitch -1 5 NATURAL))
  (b . ,(ly:make-pitch -1 6 NATURAL))

  (cc . ,(ly:make-pitch -1 0 KOMA))
  (dc . ,(ly:make-pitch -1 1 KOMA))
  (ec . ,(ly:make-pitch -1 2 KOMA))
  (fc . ,(ly:make-pitch -1 3 KOMA))
  (gc . ,(ly:make-pitch -1 4 KOMA))
  (ac . ,(ly:make-pitch -1 5 KOMA))
  (bc . ,(ly:make-pitch -1 6 KOMA))

  (ceb . ,(ly:make-pitch -1 0 EKSIK-BAKIYE))
  (deb . ,(ly:make-pitch -1 1 EKSIK-BAKIYE))
  (eeb . ,(ly:make-pitch -1 2 EKSIK-BAKIYE))
  (feb . ,(ly:make-pitch -1 3 EKSIK-BAKIYE))
  (geb . ,(ly:make-pitch -1 4 EKSIK-BAKIYE))
  (aeb . ,(ly:make-pitch -1 5 EKSIK-BAKIYE))
  (beb . ,(ly:make-pitch -1 6 EKSIK-BAKIYE))

  (cb . ,(ly:make-pitch -1 0 BAKIYE))
  (db . ,(ly:make-pitch -1 1 BAKIYE))
  (eb . ,(ly:make-pitch -1 2 BAKIYE))
  (fb . ,(ly:make-pitch -1 3 BAKIYE))
  (gb . ,(ly:make-pitch -1 4 BAKIYE))
  (ab . ,(ly:make-pitch -1 5 BAKIYE))
  (bb . ,(ly:make-pitch -1 6 BAKIYE))

  (ck . ,(ly:make-pitch -1 0 KUCUK))
  (dk . ,(ly:make-pitch -1 1 KUCUK))
  (ek . ,(ly:make-pitch -1 2 KUCUK))
  (fk . ,(ly:make-pitch -1 3 KUCUK))
  (gk . ,(ly:make-pitch -1 4 KUCUK))
  (ak . ,(ly:make-pitch -1 5 KUCUK))
  (bk . ,(ly:make-pitch -1 6 KUCUK))

  (cbm . ,(ly:make-pitch -1 0 BUYUKMUCENNEB))
  (dbm . ,(ly:make-pitch -1 1 BUYUKMUCENNEB))
  (ebm . ,(ly:make-pitch -1 2 BUYUKMUCENNEB))
  (fbm . ,(ly:make-pitch -1 3 BUYUKMUCENNEB))
  (gbm . ,(ly:make-pitch -1 4 BUYUKMUCENNEB))
  (abm . ,(ly:make-pitch -1 5 BUYUKMUCENNEB))
  (bbm . ,(ly:make-pitch -1 6 BUYUKMUCENNEB))

  (ct . ,(ly:make-pitch -1 0 TANINI))
  (dt . ,(ly:make-pitch -1 1 TANINI))
  (et . ,(ly:make-pitch -1 2 TANINI))
  (ft . ,(ly:make-pitch -1 3 TANINI))
  (gt . ,(ly:make-pitch -1 4 TANINI))
  (at . ,(ly:make-pitch -1 5 TANINI))
  (bt . ,(ly:make-pitch -1 6 TANINI))

;; f for flat.
  (cfc . ,(ly:make-pitch -1 0 (- KOMA)))
  (dfc . ,(ly:make-pitch -1 1 (- KOMA)))
  (efc . ,(ly:make-pitch -1 2 (- KOMA)))
  (ffc . ,(ly:make-pitch -1 3 (- KOMA)))
  (gfc . ,(ly:make-pitch -1 4 (- KOMA)))
  (afc . ,(ly:make-pitch -1 5 (- KOMA)))
  (bfc . ,(ly:make-pitch -1 6 (- KOMA)))

  (cfi . ,(ly:make-pitch -1 0 (- CEYREK)))
  (dfi . ,(ly:make-pitch -1 1 (- CEYREK)))
  (efi . ,(ly:make-pitch -1 2 (- CEYREK)))
  (ffi . ,(ly:make-pitch -1 3 (- CEYREK)))
  (gfi . ,(ly:make-pitch -1 4 (- CEYREK)))
  (afi . ,(ly:make-pitch -1 5 (- CEYREK)))
  (bfi . ,(ly:make-pitch -1 6 (- CEYREK)))

  (cfu . ,(ly:make-pitch -1 0 (- EKSIK-BAKIYE)))
  (dfu . ,(ly:make-pitch -1 1 (- EKSIK-BAKIYE)))
  (efu . ,(ly:make-pitch -1 2 (- EKSIK-BAKIYE)))
  (ffu . ,(ly:make-pitch -1 3 (- EKSIK-BAKIYE)))
  (gfu . ,(ly:make-pitch -1 4 (- EKSIK-BAKIYE)))
  (afu . ,(ly:make-pitch -1 5 (- EKSIK-BAKIYE)))
  (bfu . ,(ly:make-pitch -1 6 (- EKSIK-BAKIYE)))

  (cfb . ,(ly:make-pitch -1 0 (- BAKIYE)))
  (dfb . ,(ly:make-pitch -1 1 (- BAKIYE)))
  (efb . ,(ly:make-pitch -1 2 (- BAKIYE)))
  (ffb . ,(ly:make-pitch -1 3 (- BAKIYE)))
  (gfb . ,(ly:make-pitch -1 4 (- BAKIYE)))
  (afb . ,(ly:make-pitch -1 5 (- BAKIYE)))
  (bfb . ,(ly:make-pitch -1 6 (- BAKIYE)))

  (cfk . ,(ly:make-pitch -1 0 (- KUCUK)))
  (dfk . ,(ly:make-pitch -1 1 (- KUCUK)))
  (efk . ,(ly:make-pitch -1 2 (- KUCUK)))
  (ffk . ,(ly:make-pitch -1 3 (- KUCUK)))
  (gfk . ,(ly:make-pitch -1 4 (- KUCUK)))
  (afk . ,(ly:make-pitch -1 5 (- KUCUK)))
  (bfk . ,(ly:make-pitch -1 6 (- KUCUK)))

  (cfbm . ,(ly:make-pitch -1 0 (- BUYUKMUCENNEB)))
  (dfbm . ,(ly:make-pitch -1 1 (- BUYUKMUCENNEB)))
  (efbm . ,(ly:make-pitch -1 2 (- BUYUKMUCENNEB)))
  (ffbm . ,(ly:make-pitch -1 3 (- BUYUKMUCENNEB)))
  (gfbm . ,(ly:make-pitch -1 4 (- BUYUKMUCENNEB)))
  (afbm . ,(ly:make-pitch -1 5 (- BUYUKMUCENNEB)))
  (bfbm . ,(ly:make-pitch -1 6 (- BUYUKMUCENNEB)))

  (cft . ,(ly:make-pitch -1 0 (- TANINI)))
  (dft . ,(ly:make-pitch -1 1 (- TANINI)))
  (eft . ,(ly:make-pitch -1 2 (- TANINI)))
  (fft . ,(ly:make-pitch -1 3 (- TANINI)))
  (gft . ,(ly:make-pitch -1 4 (- TANINI)))
  (aft . ,(ly:make-pitch -1 5 (- TANINI)))
  (bft . ,(ly:make-pitch -1 6 (- TANINI)))

)

%% Set pitch names.
#(set! language-pitch-names
       (append language-pitch-names
               (list `(turkish-makam . ,turkishMakamPitchNames))))
\language "turkish-makam"

#(define eksikMirroredSlashedFlat
  (if (defined? 'eksikMirroredSlashedFlat)
  eksikMirroredSlashedFlat #t))

turkishMakamGlyphs = #`(
       (,TANINI  . "accidentals.doublesharp")
       (,BUYUKMUCENNEB  . "accidentals.sharp.slashslashslash.stemstem")
       (,KUCUK  . "accidentals.sharp.slashslashslash.stem")
       (,BAKIYE  . "accidentals.sharp")
       (,EKSIK-BAKIYE  . "accidentals.sharp.arrowdown")
       (,KOMA   . "accidentals.sharp.slashslash.stem")
       (0      . "accidentals.natural")
       (,(- KOMA)  . "accidentals.mirroredflat")
       (,(- EKSIK-BAKIYE) . "accidentals.mirroredflat.backslash")
       (,(- CEYREK) . "accidentals.mirroredflat.backslash")
       (,(- BAKIYE) . "accidentals.flat.slash")
       (,(- KUCUK) . "accidentals.flat")
       (,(- BUYUKMUCENNEB) . "accidentals.flat.slashslash")
       (,(- TANINI) . "accidentals.flatflat")
)

%{

key signature definitions

%}

%%% acemliyegah, similar and transpositions
acemliyegah = #`((0 . ,NATURAL)(1 . ,NATURAL)(2 . ,(- KUCUK))(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,(- KOMA))(6 . ,(- KUCUK))) %\key d
dilefruz=\acemliyegah %\key d
dilfuruz=\acemliyegah %\key d
isfahanruyineva=\acemliyegah %\key d'
arabanbuselik=\acemliyegah %\key a
nisaburrumi=\acemliyegah %\key e

%%% arazbar, similar and transpositions
arazbar = #`((0 . ,NATURAL)(1 . ,(- KOMA))(2 . ,(- KUCUK))(3 . ,NATURAL)(4 . ,(- KOMA))(5 . ,(- KUCUK))(6 . ,(- KUCUK))) %\key a
arazbarbuselik=\arazbar %\key a
arazbarzemzeme=\arazbar %\key a
huseyniaraban=\arazbar %\key e
huseyniasiran=\arazbar %\key e
irakasiran=\arazbar %\key e
mayeasiran=\arazbar %\key e
nuhuft=\arazbar %\key e
ussakasiran=\arazbar %\key e

%%% bahrinazik and similar
bahrinazik = #`((0 . ,NATURAL)(1 . ,(- KOMA))(2 . ,(- KOMA))(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,(- KUCUK))(6 . ,(- KUCUK))) %\key a

%%% bendihisar and similar
bendihisar = #`((0 . ,BAKIYE)(1 . ,NATURAL)(2 . ,(- KUCUK))(3 . ,BAKIYE)(4 . ,NATURAL)(5 . ,NATURAL)(6 . ,(- KUCUK))) %\key d

%%% bestehicaz and similar, uses -EKSIK-BAKIYE at 3
bestehicaz = #`((1 . ,(- BAKIYE))(2 . ,(- BAKIYE))(3 . ,(- EKSIK-BAKIYE))(4 . ,NATURAL)(5 . ,(- BAKIYE))(6 . ,(- BAKIYE))) %\key fb
sehnazhaveran=\bestehicaz %\key fb

%%% bestenigar and similar, uses - BUYUKMUCENNEB at 5
bestenigar = #`((1 . ,(- BAKIYE))(2 . ,(- BAKIYE))(3 . ,NATURAL)(4 . ,(- BAKIYE))(5 . ,(- BUYUKMUCENNEB))(6 . ,(- BAKIYE))) %\key fb
feyziyekta=\bestenigar %\key fb

%%% buselik, similar and transpositions
buselik = #`((0 . ,NATURAL)(1 . ,NATURAL)(2 . ,(- KUCUK))(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,(- KUCUK))(6 . ,(- KUCUK))) %\key a
arazbarek=\buselik %\key a
beyatiruyiacem=\buselik %\key a
fethidil=\buselik %\key a
hisarbuselik=\buselik %\key a
nevruz=\buselik %\key a
nevruzirumi=\buselik %\key a
sehnazbuselik=\buselik %\key a
ferahfeza=\buselik %\key d
muberka=\buselik %\key d
tarabengiz=\buselik %\key d
ruhnuvaz=\buselik %\key e
zirefkend=\buselik %\key e
bezmitarab=\buselik %\key g
nihavend=\buselik %\key g
nihavendikebir=\buselik %\key g
tavrinihavend=\buselik %\key g

%%% buzurk and similar
buzurk = #`((0 . ,NATURAL)(1 . ,NATURAL)(2 . ,NATURAL)(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,NATURAL)(6 . ,(- KOMA))) %\key g

%%% canfeza and similar, uses -TANINI at 6
canfeza = #`((0 . ,NATURAL)(1 . ,(- KUCUK))(2 . ,(- KUCUK))(3 . ,NATURAL)(4 . ,(- KOMA))(5 . ,(- KUCUK))(6 . ,(- TANINI))) %\key e
sabaasiran=\canfeza %\key e

%%% cargah, similar and transpositions
cargah = #`((0 . ,NATURAL)(1 . ,NATURAL)(2 . ,NATURAL)(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,NATURAL)(6 . ,NATURAL)) %\key c
acemasiran=\cargah %\key f
tarzicedid=\cargah %\key f
tarzicihan=\cargah %\key f
mahur=\cargah %\key g
suzidilara=\cargah %\key g
zavil=\cargah %\key g

%%% cargaheski and similar
cargaheski = #`((0 . ,NATURAL)(1 . ,(- BAKIYE))(2 . ,NATURAL)(4 . ,NATURAL)(5 . ,NATURAL)(6 . ,(- KOMA))) %\key c

%%% evcara and similar
evcara = #`((0 . ,NATURAL)(1 . ,(- BAKIYE))(2 . ,NATURAL)(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,(- BAKIYE))(6 . ,NATURAL)) %\key fb

%%% evcisevk and similar
evcisevk = #`((0 . ,BAKIYE)(1 . ,(- KOMA))(2 . ,(- KUCUK))(3 . ,BAKIYE)(4 . ,(- KOMA))(5 . ,(- KOMA))(6 . ,(- KUCUK))) %\key e

%%% evic and similar
evic = #`((0 . ,NATURAL)(1 . ,(- BAKIYE))(2 . ,(- BAKIYE))(3 . ,NATURAL)(4 . ,(- BAKIYE))(5 . ,(- BAKIYE))(6 . ,(- BAKIYE))) %\key fb
dilkeshaveran=\evic %\key fb
evicbuselik=\evic %\key fb
evicmaye=\evic %\key fb
evciruyineva=\evic %\key fb
gulbuse=\evic %\key fb
irak=\evic %\key fb
nevadilkes=\evic %\key fb
rastasiran=\evic %\key fb
rasthaveran=\evic %\key fb
ruyiirak=\evic %\key fb

%%% ferahnak and similar
ferahnak = #`((0 . ,NATURAL)(1 . ,(- BAKIYE))(2 . ,(- BAKIYE))(3 . ,KOMA)(4 . ,NATURAL)(5 . ,(- BAKIYE))(6 . ,(- BAKIYE))) %\key fb
neveda=\ferahnak %\key fb

%%% gevest and similar
gevest = #`((0 . ,NATURAL)(1 . ,(- KUCUK))(2 . ,(- KUCUK))(3 . ,NATURAL)(4 . ,(- KUCUK))(5 . ,(- KUCUK))(6 . ,(- KUCUK))) %\key fk

%%% guldeste and similar
guldeste = #`((0 . ,NATURAL)(1 . ,NATURAL)(2 . ,NATURAL)(3 . ,NATURAL)(4 . ,BAKIYE)(5 . ,NATURAL)(6 . ,NATURAL)) %\key g

%%% hicaz, similar and transpositions
hicaz = #`((0 . ,NATURAL)(1 . ,(- BAKIYE))(2 . ,(- KOMA))(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,(- KOMA))(6 . ,(- KUCUK))) %\key a
hicazhumayun = #`((0 . ,NATURAL)(1 . ,(- BAKIYE))(2 . ,(- KOMA))(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,(- KUCUK))(6 . ,(- KUCUK))) %\key a
hicazzirgule = #`((0 . ,NATURAL)(1 . ,(- BAKIYE))(2 . ,(- KOMA))(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,(- BAKIYE))(6 . ,(- KOMA))) %\key a
hicazbuselik=\hicaz %\key a
hicazuzzal=\hicaz %\key a
hicazzemzeme=\hicaz %\key a
sehnaz=\hicazhumayun %\key a
uzzal=\hicazuzzal %\key a
lalegul=\hicaz %\key d
sedaraban=\hicazzirgule %\key d
sultanicedid=\hicazzirgule %\key d
hicazkar=\hicazzirgule %\key g
zevkidil=\hicazzirgule %\key g
zirgulelisuznak=\hicazzirgule %\key g

%%% huzzam, similar and transpositions, uses -EKSIK-BAKIYE at 3
huzzam = #`((0 . ,NATURAL)(1 . ,(- BAKIYE))(2 . ,(- BAKIYE))(3 . ,(- EKSIK-BAKIYE))(4 . ,NATURAL)(5 . ,(- BAKIYE))(6 . ,(- BAKIYE))) %\key bfc
evcibahrinazik=\huzzam %\key fb
hicazirak=\huzzam %\key fb
muhalifiirak=\huzzam %\key fb
rahatulervah=\huzzam %\key fb

%%% karcigar, similar and transpositions
karcigar = #`((0 . ,NATURAL)(1 . ,(- KOMA))(2 . ,(- KUCUK))(3 . ,NATURAL)(4 . ,(- BAKIYE))(5 . ,(- KOMA))(6 . ,(- KUCUK))) %\key a
beyatiaraban=\karcigar %\key a
beyatiarabanbuselik=\karcigar %\key a
hicazasiran=\karcigar %\key e
hicazeyn=\karcigar %\key e

%%% kurdi, similar and transpositions
kurdi = #`((0 . ,NATURAL)(1 . ,(- KUCUK))(2 . ,(- KUCUK))(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,(- KUCUK))(6 . ,(- KUCUK))) %\key a
acemkurdi=\kurdi %\key a
muhayyerkurdi=\kurdi %\key a
mahurhan=\kurdi %b
anberefsan=\kurdi %\key d
ferahnuma=\kurdi %\key d
askevza=\kurdi %\key e
kurdilihicazkar=\kurdi %\key g

%%% mahurbuselik, similar and transpositions
mahurbuselik = #`((0 . ,NATURAL)(1 . ,NATURAL)(2 . ,(- KUCUK))(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,NATURAL)(6 . ,(- KUCUK))) %\key a
dilnevaz=\mahurbuselik %\key d
vecdidil=\mahurbuselik %\key d

%%% neva, similar and transpositions
neva = #`((0 . ,NATURAL)(1 . ,(- KOMA))(2 . ,(- KUCUK))(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,(- KOMA))(6 . ,(- KUCUK))) %\key a
evichuzi=\neva %\key a
gerdaniye=\neva %\key a
gerdaniyebuselik=\neva %\key a
gerdaniyekurdi=\neva %\key a
gulizar=\neva %\key a
horasan=\neva %\key a
huseyni=\neva %\key a
huseynizemzeme=\neva %\key a
huzi=\neva %\key a
muhayyer=\neva %\key a
muhayyerbuselik=\neva %\key a
muhayyerzirgule=\neva %\key a
nevabuselik=\neva %\key a
nevakurdi=\neva %\key a
sultaniirak=\neva %\key a
tahir=\neva %\key a
tahirbuselik=\neva %\key a
ferahnakasiran=\neva %\key e

%%% neveser and similar
neveser = #`((0 . ,NATURAL)(1 . ,NATURAL)(2 . ,(- BAKIYE))(3 . ,BAKIYE)(4 . ,NATURAL)(5 . ,(- BAKIYE))(6 . ,(- KOMA))) %\key g

%%% nigar and similar
nigar = #`((0 . ,NATURAL)(1 . ,NATURAL)(2 . ,NATURAL)(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,NATURAL)(6 . ,(- KUCUK))) %\key g

%%% nikriz and similar
nikriz = #`((0 . ,NATURAL)(1 . ,NATURAL)(2 . ,(- BAKIYE))(3 . ,BAKIYE)(4 . ,NATURAL)(5 . ,NATURAL)(6 . ,(- KUCUK))) %\key g
maveraunnehir=\nikriz %\key g

%%% nisabur and similar
nisabur = #`((0 . ,NATURAL)(1 . ,(- KOMA))(2 . ,(- KUCUK))(3 . ,NATURAL)(4 . ,(- KUCUK))(5 . ,(- KUCUK))(6 . ,(- KUCUK))) %b

%%% nisaburek, similar and transpositions
nisaburek = #`((0 . ,NATURAL)(1 . ,NATURAL)(2 . ,(- KOMA))(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,NATURAL)(6 . ,(- KUCUK))) %\key a
nevabagdat=\nisaburek %\key d'
bestehisar=\nisaburek %\key g
fethibelgrad=\nisaburek %\key g
muhalif=\nisaburek %\key g
ruyidilara=\nisaburek %\key g
sevkicihan=\nisaburek %\key g
tebriz=\nisaburek %\key g

%%% peykisafa and similar
peykisafa = #`((0 . ,NATURAL)(1 . ,NATURAL)(2 . ,(- KOMA))(3 . ,NATURAL)(4 . ,(- BAKIYE))(5 . ,NATURAL)(6 . ,(- KUCUK))) %\key g

%%% rast, similar and transpositions
rast = #`((0 . ,NATURAL)(1 . ,NATURAL)(2 . ,(- KOMA))(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,NATURAL)(6 . ,(- KOMA))) % \key g

aramican=\rast %\key g
dilnisin=\rast %\key g
gestuguzaribahar=\rast %\key g
hosaver=\rast %\key g
nevkes=\rast %\key g
pencgah=\rast %\key g
pesendide=\rast %\key g
peykinesat=\rast %\key g
rasticedid=\rast %\key g
rastisagir=\rast %\key g
rastmaye=\rast %\key g
rehavi=\rast %\key g
sazkar=\rast %\key g
selmek=\rast %\key g
sevkidil=\rast %\key g
dilkusa=\rast %\key d'

%%% rekbizavil and similar
rekbizavil = #`((0 . ,NATURAL)(1 . ,NATURAL)(2 . ,(- BAKIYE))(3 . ,BAKIYE)(4 . ,NATURAL)(5 . ,NATURAL)(6 . ,(- KOMA))) %\key g

%%% rengidil and similar
rengidil = #`((0 . ,NATURAL)(1 . ,NATURAL)(2 . ,NATURAL)(3 . ,BAKIYE)(4 . ,NATURAL)(5 . ,(- BAKIYE))(6 . ,(- KOMA))) %\key f

%%% revnaknuma and similar
revnaknuma = #`((1 . ,(- BAKIYE))(2 . ,NATURAL)(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,(- BAKIYE))(6 . ,(- BAKIYE))) %\key fb

%%% saba and similar
saba = #`((0 . ,NATURAL)(1 . ,(- KOMA))(2 . ,(- KUCUK))(3 . ,(- BAKIYE))(4 . ,NATURAL)(5 . ,(- KUCUK))(6 . ,(- KUCUK))) %\key a
dugah=\saba %\key a
dugahbuselik=\saba %\key a
eskisipihr=\saba %\key a
kucek=\saba %\key a
muhayyersunbule=\saba %\key a
sababuselik=\saba %\key a
sabazemzeme=\saba %\key a
sipihr=\saba %\key a

%%% segah and similar
segah = #`((0 . ,NATURAL)(1 . ,(- BAKIYE))(2 . ,(- BAKIYE))(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,(- BAKIYE))(6 . ,(- BAKIYE))) %\key bfc
mustear=\segah %\key bfc
segahmaye=\segah %\key bfc
vechiarazbar=\segah %\key bfc

%%% sehnazasiran, similar and transpositions
sehnazasiran = #`((0 . ,NATURAL)(1 . ,NATURAL)(2 . ,NATURAL)(3 . ,KUCUK)(4 . ,NATURAL)(5 . ,NATURAL)(6 . ,NATURAL)) %\key f
narefte=\sehnazasiran %\key bfk

%%% sevkefza and similar
sevkefza = #`((0 . ,NATURAL)(1 . ,NATURAL)(2 . ,NATURAL)(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,(- BAKIYE))(6 . ,NATURAL)) %\key f

%%% sevkaver and similar
sevkaver = #`((0 . ,NATURAL)(1 . ,NATURAL)(2 . ,NATURAL)(3 . ,BAKIYE)(4 . ,NATURAL)(5 . ,NATURAL)(6 . ,(- KOMA))) %\key f

%%% sevkitarab and similar
sevkitarab = #`((0 . ,NATURAL)(1 . ,NATURAL)(2 . ,NATURAL)(3 . ,BAKIYE)(4 . ,NATURAL)(5 . ,(- BAKIYE))(6 . ,NATURAL)) %\key f
sevkutarab=\sevkitarab %\key f

%%% sivenuma and similar
sivenuma = #`((0 . ,NATURAL)(1 . ,NATURAL)(2 . ,(- KUCUK))(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,(- KOMA))(6 . ,(- KUCUK))(7 . ,(- BAKIYE))) %\key d

%%% sultanisegah and similar
sultanisegah = #`((0 . ,NATURAL)(1 . ,(- KOMA))(2 . ,(- KOMA))(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,(- KOMA))(6 . ,(- KUCUK))) %\key d
segaharaban=\sultanisegah %\key d

%%% sultaniyegah and similar
sultaniyegah = #`((0 . ,NATURAL)(1 . ,NATURAL)(2 . ,(- KUCUK))(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,(- BAKIYE))(6 . ,(- KOMA))) %\key d

%%% suzidil and similar
suzidil = #`((0 . ,NATURAL)(1 . ,(- BAKIYE))(2 . ,(- KOMA))(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,(- KUCUK))(6 . ,(- KOMA))) %\key e
sevkiserab=\suzidil %\key e

%%% suznak, similar and transpositions
suznak = #`((0 . ,NATURAL)(1 . ,NATURAL)(2 . ,(- KOMA))(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,(- BAKIYE))(6 . ,(- KOMA))) %\key g
serefnuma=\suznak %\key d
gulsenivefa=\suznak %\key d

%%% tarzinevin and similar
tarzinevin = #`((0 . ,NATURAL)(1 . ,NATURAL)(2 . ,(- KUCUK))(3 . ,NATURAL)(4 . ,(- BAKIYE))(5 . ,NATURAL)(6 . ,(- KUCUK))) %\key g

%%% ussak, similar and transpositions
ussak = #`((0 . ,NATURAL)(1 . ,(- KOMA))(2 . ,(- KUCUK))(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,(- KUCUK))(6 . ,(- KUCUK))) %\key a
acem=\ussak %\key a
acembuselik=\ussak %\key a
besteisfahan=\ussak %\key a
beyati=\ussak %\key a
beyatibuselik=\ussak %\key a
buselikasiran=\ussak %\key a
dugahmaye=\ussak %\key a
hisar=\ussak %\key a
hisarvechisehnaz=\ussak %\key a
isfahan=\ussak %\key a
isfahanek=\ussak %\key a
yenisipihr=\ussak %\key a
zavilasiran=\ussak %\key e

%%% yegah and similar
yegah = #`((0 . ,NATURAL)(1 . ,NATURAL)(2 . ,(- KOMA))(3 . ,NATURAL)(4 . ,NATURAL)(5 . ,(- KOMA))(6 . ,(- KUCUK))) %\key d
dilkes=\yegah %\key d
dilkeside=\yegah %\key d
sultanirast=\yegah %\key d

\paper {
  font-defaults.alteration-glyph-name-alist = \turkishMakamGlyphs
}

\layout {
  \context {
    \Score

%{

key alteration order

%}

keyAlterationOrder = #`(
(6 . ,(- KOMA)) (6 . ,(- CEYREK)) (6 . ,(- EKSIK-BAKIYE)) (6 . ,(- BAKIYE)) (6 . ,(- KUCUK)) (6 . ,(- BUYUKMUCENNEB)) (6 . ,(- TANINI))
(2 . ,(- KOMA)) (2 . ,(- CEYREK)) (2 . ,(- EKSIK-BAKIYE)) (2 . ,(- BAKIYE)) (2 . ,(- KUCUK)) (2 . ,(- BUYUKMUCENNEB)) (2 . ,(- TANINI))
(5 . ,(- KOMA)) (5 . ,(- CEYREK)) (5 . ,(- EKSIK-BAKIYE)) (5 . ,(- BAKIYE)) (5 . ,(- KUCUK)) (5 . ,(- BUYUKMUCENNEB)) (5 . ,(- TANINI))
(1 . ,(- KOMA)) (1 . ,(- CEYREK)) (1 . ,(- EKSIK-BAKIYE)) (1 . ,(- BAKIYE)) (1 . ,(- KUCUK)) (1 . ,(- BUYUKMUCENNEB)) (1 . ,(- TANINI))
(4 . ,(- KOMA)) (4 . ,(- CEYREK)) (4 . ,(- EKSIK-BAKIYE)) (4 . ,(- BAKIYE)) (4 . ,(- KUCUK)) (4 . ,(- BUYUKMUCENNEB)) (4 . ,(- TANINI))
(0 . ,(- KOMA)) (0 . ,(- CEYREK)) (0 . ,(- EKSIK-BAKIYE)) (0 . ,(- BAKIYE)) (0 . ,(- KUCUK)) (0 . ,(- BUYUKMUCENNEB)) (0 . ,(- TANINI))
(3 . ,(- KOMA)) (3 . ,(- CEYREK)) (3 . ,(- EKSIK-BAKIYE)) (3 . ,(- BAKIYE)) (3 . ,(- KUCUK)) (3 . ,(- BUYUKMUCENNEB)) (3 . ,(- TANINI))

(3 . ,KOMA) (3 . ,CEYREK) (3 . ,EKSIK-BAKIYE) (3 . ,BAKIYE) (3 . ,KUCUK) (3 . ,BUYUKMUCENNEB) (3 . ,TANINI)
(0 . ,KOMA) (0 . ,CEYREK) (0 . ,EKSIK-BAKIYE) (0 . ,BAKIYE) (0 . ,KUCUK) (0 . ,BUYUKMUCENNEB) (0 . ,TANINI)
(4 . ,KOMA) (4 . ,CEYREK) (4 . ,EKSIK-BAKIYE) (4 . ,BAKIYE) (4 . ,KUCUK) (4 . ,BUYUKMUCENNEB) (4 . ,TANINI)
(1 . ,KOMA) (1 . ,CEYREK) (1 . ,EKSIK-BAKIYE) (1 . ,BAKIYE) (1 . ,KUCUK) (1 . ,BUYUKMUCENNEB) (1 . ,TANINI)
(5 . ,KOMA) (5 . ,CEYREK) (5 . ,EKSIK-BAKIYE) (5 . ,BAKIYE) (5 . ,KUCUK) (5 . ,BUYUKMUCENNEB) (5 . ,TANINI)
(2 . ,KOMA) (2 . ,CEYREK) (2 . ,EKSIK-BAKIYE) (2 . ,BAKIYE) (2 . ,KUCUK) (2 . ,BUYUKMUCENNEB) (2 . ,TANINI)
(6 . ,KOMA) (6 . ,CEYREK) (6 . ,EKSIK-BAKIYE) (6 . ,BAKIYE) (6 . ,KUCUK) (6 . ,BUYUKMUCENNEB) (6 . ,TANINI)
)

%{

key signature padding pairs

%}

\override KeySignature.padding-pairs = #'(
  (("accidentals.natural" . "accidentals.natural") . 0.2)

  (("accidentals.flat" . "accidentals.flat") . 0.1)
  (("accidentals.flat" . "accidentals.flat.slash") . 0.4)
  (("accidentals.flat" . "accidentals.mirroredflat") . 0.3)
  (("accidentals.flat" . "accidentals.flat.slashslash") . 0.0)

  (("accidentals.flat.slash" . "accidentals.flat") . 0.0)
  (("accidentals.flat.slash" . "accidentals.flat.slash") . 0.0)
  (("accidentals.flat.slash" . "accidentals.mirroredflat") . 0.0)
  (("accidentals.flat.slash" . "accidentals.flat.slashslash") . 0.0)

  (("accidentals.mirroredflat" . "accidentals.flat") . 0.5)
  (("accidentals.mirroredflat" . "accidentals.flat.slash") . 0.4)
  (("accidentals.mirroredflat" . "accidentals.mirroredflat") . 0.3)
  (("accidentals.mirroredflat" . "accidentals.flat.slashslash") . 0.0)

  (("accidentals.flat.slashslash" . "accidentals.flat") . 0.0)
  (("accidentals.flat.slashslash" . "accidentals.flat.slash") . 0.3)
  (("accidentals.flat.slashslash" . "accidentals.mirroredflat") . 0.3)
  (("accidentals.flat.slashslash" . "accidentals.flat.slashslash") . -0.2)

  (("accidentals.sharp" . "accidentals.sharp") . 0.2)
  (("accidentals.sharp" . "accidentals.sharp.slashslash.stem") . 0.0)
  (("accidentals.sharp" . "accidentals.sharp.slashslashslash.stem") . 0.0)
  (("accidentals.sharp" . "accidentals.slashslashslash.stemstem") . 0.0)

  (("accidentals.sharp.slashslash.stem" . "accidentals.sharp") . 0.4)
  (("accidentals.sharp.slashslash.stem" . "accidentals.sharp.slashslash.stem") . 0.2)
  (("accidentals.sharp.slashslash.stem" . "accidentals.sharp.slashslashslash.stem") . 0.0)
  (("accidentals.sharp.slashslash.stem" . "accidentals.slashslashslash.stemstem") . 0.0)

  (("accidentals.sharp.slashslashslash.stem" . "accidentals.sharp") . 0.2)
  (("accidentals.sharp.slashslashslash.stem" . "accidentals.sharp.slashslash.stem") . 0.2)
  (("accidentals.sharp.slashslashslash.stem" . "accidentals.sharp.slashslashslash.stem") . 0.0)
  (("accidentals.sharp.slashslashslash.stem" . "accidentals.slashslashslash.stemstem") . 0.0)

  (("accidentals.sharp.slashslashslash.stemstem" . "accidentals.sharp") . 0.1)
  (("accidentals.sharp.slashslashslash.stemstem" . "accidentals.sharp.slashslash.stem") . 0.0)
  (("accidentals.sharp.slashslashslash.stemstem" . "accidentals.sharp.slashslashslash.stem") . 0.0)
  (("accidentals.sharp.slashslashslash.stemstem" . "accidentals.slashslashslash.stemstem") . 0.0)

  (("accidentals.flat" . "accidentals.sharp") . 0.4)
  (("accidentals.flat" . "accidentals.sharp.slashslash.stem") . 0.0)
  (("accidentals.flat" . "accidentals.sharp.slashslashslash.stem") . 0.0)
  (("accidentals.flat" . "accidentals.slashslashslash.stemstem") . 0.0)

  (("accidentals.flat.slash" . "accidentals.sharp") . 0.4)
  (("accidentals.flat.slash" . "accidentals.sharp.slashslash.stem") . 0.1)
  (("accidentals.flat.slash" . "accidentals.sharp.slashslashslash.stem") . 0.3)
  (("accidentals.flat.slash" . "accidentals.slashslashslash.stemstem") . 0.0)

  (("accidentals.mirroredflat" . "accidentals.sharp") . 0.5)
  (("accidentals.mirroredflat" . "accidentals.sharp.slashslash.stem") . 0.0)
  (("accidentals.mirroredflat" . "accidentals.sharp.slashslashslash.stem") . 0.0)
  (("accidentals.mirroredflat" . "accidentals.slashslashslash.stemstem") . 0.0)

  (("accidentals.flat.slashslash" . "accidentals.sharp") . 0.0)
  (("accidentals.flat.slashslash" . "accidentals.sharp.slashslash.stem") . 0.0)
  (("accidentals.flat.slashslash" . "accidentals.sharp.slashslashslash.stem") . 0.0)
  (("accidentals.flat.slashslash" . "accidentals.slashslashslash.stemstem") . 0.0)

  (("accidentals.sharp" . "accidentals.flat") . 0.0)
  (("accidentals.sharp" . "accidentals.flat.slash") . 0.0)
  (("accidentals.sharp" . "accidentals.mirroredflat") . 0.0)
  (("accidentals.sharp" . "accidentals.flat.slashslash") . 0.0)

  (("accidentals.sharp.slashslash.stem" . "accidentals.flat") . 0.0)
  (("accidentals.sharp.slashslash.stem" . "accidentals.flat.slash") . 0.0)
  (("accidentals.sharp.slashslash.stem" . "accidentals.mirroredflat") . 0.0)
  (("accidentals.sharp.slashslash.stem" . "accidentals.flat.slashslash") . 0.0)

  (("accidentals.sharp.slashslashslash.stem" . "accidentals.flat") . 0.0)
  (("accidentals.sharp.slashslashslash.stem" . "accidentals.flat.slash") . 0.0)
  (("accidentals.sharp.slashslashslash.stem" . "accidentals.mirroredflat") . 0.0)
  (("accidentals.sharp.slashslashslash.stem" . "accidentals.flat.slashslash") . 0.0)

  (("accidentals.sharp.slashslashslash.stemstem" . "accidentals.flat") . 0.0)
  (("accidentals.sharp.slashslashslash.stemstem" . "accidentals.flat.slash") . 0.0)
  (("accidentals.sharp.slashslashslash.stemstem" . "accidentals.mirroredflat") . 0.0)
  (("accidentals.sharp.slashslashslash.stemstem" . "accidentals.flat.slashslash") . 0.0)
)
  }
}

