% script-init.ly

\version "2.17.25"

% code char abbreviations
dashHat = "marcato"
dashPlus = "stopped"
dashDash = "tenuto"
dashBang = "staccatissimo"
dashLarger = "accent"
dashDot = "staccato"
dashUnderscore = "portato"

harmonic = #(make-music 'HarmonicEvent)

accent = #(make-articulation "accent")
coda = #(make-articulation "coda")
downbow = #(make-articulation "downbow")
downmordent = #(make-articulation "downmordent")
downprall = #(make-articulation "downprall")
espressivo = #(make-articulation "espressivo")
fermata = #(make-articulation "fermata")
flageolet = #(make-articulation "flageolet")
halfopen = #(make-articulation "halfopen")
lheel = #(make-articulation "lheel")
lineprall = #(make-articulation "lineprall")
longfermata = #(make-articulation "longfermata")
ltoe = #(make-articulation "ltoe")
marcato = #(make-articulation "marcato")
mordent = #(make-articulation "mordent")
open = #(make-articulation "open")
portato = #(make-articulation "portato")
prall = #(make-articulation "prall")
pralldown = #(make-articulation "pralldown")
prallmordent = #(make-articulation "prallmordent")
prallprall = #(make-articulation "prallprall")
prallup = #(make-articulation "prallup")
reverseturn = #(make-articulation "reverseturn")
rheel = #(make-articulation "rheel")
rtoe = #(make-articulation "rtoe")
segno = #(make-articulation "segno")
shortfermata = #(make-articulation "shortfermata")
signumcongruentiae = #(make-articulation "signumcongruentiae")
snappizzicato = #(make-articulation "snappizzicato")
staccatissimo = #(make-articulation "staccatissimo")
staccato = #(make-articulation "staccato")
stopped = #(make-articulation "stopped")
tenuto = #(make-articulation "tenuto")
thumb = \finger \markup \scale #(cons (magstep 5) (magstep 5))
                        \musicglyph #"scripts.thumb"
trill = #(make-articulation "trill")
turn = #(make-articulation "turn")
upbow = #(make-articulation "upbow")
upmordent = #(make-articulation "upmordent")
upprall = #(make-articulation "upprall")
varcoda = #(make-articulation "varcoda")
verylongfermata = #(make-articulation "verylongfermata")
