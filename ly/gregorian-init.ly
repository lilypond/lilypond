\version "1.9.1"

%%%%%%%%
%%%%%%%% shortcuts common for all styles of gregorian chant notation
%%%%%%%%

%
% declare head prefix shortcuts
%
virga =
  \once \property Voice.NoteHead \override #'virga = ##t
stropha =
  \once \property Voice.NoteHead \override #'stropha = ##t
inclinatum =
  \once \property Voice.NoteHead \override #'inclinatum = ##t
auctum =
  \once \property Voice.NoteHead \override #'auctum = ##t
descendens =
  \once \property Voice.NoteHead \override #'descendens = ##t
ascendens =
  \once \property Voice.NoteHead \override #'ascendens = ##t
pes =
  \once \property Voice.NoteHead \override #'pes-or-flexa = ##t
flexa =
  \once \property Voice.NoteHead \override #'pes-or-flexa = ##t
oriscus =
  \once \property Voice.NoteHead \override #'oriscus = ##t
quilisma =
  \once \property Voice.NoteHead \override #'quilisma = ##t
deminutum =
  \once \property Voice.NoteHead \override #'deminutum = ##t

%
% declare divisiones shortcuts
%
virgula = {
  \once \property Voice.BreathingSign \override #'text = #"scripts-rcomma"
  \once \property Voice.BreathingSign \override #'font-relative-size = #-1

  % Workaround: add padding.  Correct fix would be spacing engine handle this.
  \once \property Voice.BreathingSign \override #'extra-X-extent = #'(-1.0 . 0)

  \breathe
}
caesura = {
  \once \property Voice.BreathingSign \override #'text = #"scripts-rvarcomma"
  \once \property Voice.BreathingSign \override #'font-relative-size = #-1

  % Workaround: add padding.  Correct fix would be spacing engine handle this.
  \once \property Voice.BreathingSign \override #'extra-X-extent = #'(-1.0 . 0)

  \breathe
}
divisioMinima = {
  \once \property Voice.BreathingSign \override #'molecule-callback = #Breathing_sign::divisio_minima

  % Workaround: add padding.  Correct fix would be spacing engine handle this.
  \once \property Voice.BreathingSign \override #'extra-X-extent = #'(-1.0 . 0)

  \breathe
}
divisioMaior = {
  \once \property Voice.BreathingSign \override #'molecule-callback = #Breathing_sign::divisio_maior
  \once \property Voice.BreathingSign \override #'Y-offset-callbacks = #'()

  % Workaround: add padding.  Correct fix would be spacing engine handle this.
  \once \property Voice.BreathingSign \override #'extra-X-extent = #'(-1.0 . 0)

  \breathe
}
divisioMaxima = {
  \once \property Voice.BreathingSign \override #'molecule-callback = #Breathing_sign::divisio_maxima
  \once \property Voice.BreathingSign \override #'Y-offset-callbacks = #'()

  % Workaround: add padding.  Correct fix would be spacing engine handle this.
  \once \property Voice.BreathingSign \override #'extra-X-extent = #'(-1.0 . 0)

  \breathe
}
finalis = {
  \once \property Voice.BreathingSign \override #'molecule-callback = #Breathing_sign::finalis
  \once \property Voice.BreathingSign \override #'Y-offset-callbacks = #'()

  % Workaround: add padding.  Correct fix would be spacing engine handle this.
  \once \property Voice.BreathingSign \override #'extra-X-extent = #'(-1.0 . 0)

  \breathe
}

%
% declare articulation shortcuts
%
accentus = #(make-articulation "accentus")
ictus = #(make-articulation "ictus")
semicirculus = #(make-articulation "semicirculus")
circulus = #(make-articulation "circulus")
episemInitium = #(make-span-event 'TextSpanEvent START)
episemFinis = #(make-span-event 'TextSpanEvent STOP)
