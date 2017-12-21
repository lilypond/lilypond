% script-init.ly

\version "2.19.80"

harmonic = #(make-music 'HarmonicEvent)

accent = #(make-articulation "accent"
           'midi-extra-velocity 20)
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
marcato = #(make-articulation "marcato"
            'midi-extra-velocity 40)
mordent = #(make-articulation "mordent")
open = #(make-articulation "open")

portato = #(make-articulation "portato"
            'midi-length
            (lambda (len context)
             (ly:moment-mul len (ly:make-moment 3/4))))
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
staccatissimo = #(make-articulation "staccatissimo"
                  'midi-length
                  (lambda (len context)
                    (seconds->moment 1/8 context))
                  'midi-extra-velocity 6)
staccato = #(make-articulation "staccato"
             'midi-length
             (lambda (len context)
               (moment-min (ly:moment-mul len (ly:make-moment 1/2))
                           (seconds->moment 1/2 context)))
             'midi-extra-velocity 4)
stopped = #(make-articulation "stopped")
tenuto = #(make-articulation "tenuto")
thumb = \finger \markup \scale #(cons (magstep 5) (magstep 5))
                        \musicglyph "scripts.thumb"
trill = #(make-articulation "trill")
turn = #(make-articulation "turn")
upbow = #(make-articulation "upbow")
upmordent = #(make-articulation "upmordent")
upprall = #(make-articulation "upprall")
varcoda = #(make-articulation "varcoda")
verylongfermata = #(make-articulation "verylongfermata")

% code char abbreviations
dashHat = \marcato
dashPlus = \stopped
dashDash = \tenuto
dashBang = \staccatissimo
dashLarger = \accent
dashDot = \staccato
dashUnderscore = \portato
