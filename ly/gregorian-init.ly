\version "1.7.16"

%%%%%%%%
%%%%%%%% shortcuts common for all styles of gregorian chant notation
%%%%%%%%

%
% declare head prefix shortcuts
%
virga =
  \once \property Voice.LigatureHead \override #'virga = ##t
stropha =
  \once \property Voice.LigatureHead \override #'stropha = ##t
inclinatum =
  \once \property Voice.LigatureHead \override #'inclinatum = ##t
auctum =
  \once \property Voice.LigatureHead \override #'auctum = ##t
aucta =
  \once \property Voice.LigatureHead \override #'auctum = ##t
descendens =
  \once \property Voice.LigatureHead \override #'descendens = ##t
ascendens =
  \once \property Voice.LigatureHead \override #'ascendens = ##t
pes =
  \once \property Voice.LigatureHead \override #'pes-or-flexa = ##t
flexa =
  \once \property Voice.LigatureHead \override #'pes-or-flexa = ##t
semivocalis =
  \once \property Voice.LigatureHead \override #'semivocalis = ##t
oriscus =
  \once \property Voice.LigatureHead \override #'oriscus = ##t
quilisma =
  \once \property Voice.LigatureHead \override #'quilisma = ##t
deminutum =
  \once \property Voice.LigatureHead \override #'deminutum = ##t

%
% declare divisiones shortcuts
%
virgula = {
  \once \property Voice.BreathingSign \override #'text = #"scripts-rcomma"
  \once \property Voice.BreathingSign \override #'font-relative-size = #-1
  \breathe
}
caesura = {
  \once \property Voice.BreathingSign \override #'text = #"scripts-rvarcomma"
  \once \property Voice.BreathingSign \override #'font-relative-size = #-1
  \breathe
}
divisioMinima = {
  \once \property Voice.BreathingSign \override #'molecule-callback = #Breathing_sign::divisio_minima
  \breathe
}
divisioMaior = {
  \once \property Voice.BreathingSign \override #'molecule-callback = #Breathing_sign::divisio_maior
  \once \property Voice.BreathingSign \override #'Y-offset-callbacks = #'()
  \breathe
}
divisioMaxima = {
  \once \property Voice.BreathingSign \override #'molecule-callback = #Breathing_sign::divisio_maxima
  \once \property Voice.BreathingSign \override #'Y-offset-callbacks = #'()
  \breathe
}
finalis = {
  \once \property Voice.BreathingSign \override #'molecule-callback = #Breathing_sign::finalis
  \once \property Voice.BreathingSign \override #'Y-offset-callbacks = #'()
  \breathe
}

episemInitium = #(make-span-event 'TextSpanEvent START)

episemFinis = #(make-span-event 'TextSpanEvent STOP)
