\version "2.17.6"

\header {
  texidoc ="@cindex Feta scripts
This chart shows all articulations, or scripts, that the feta font contains.
"
}

#(begin

  (define script-list (map car default-script-alist))

  (define ignore
    '("comma"
      "varcomma"))

  (define ancient
    '("ictus"
      "accentus"
      "circulus"
      "semicirculus"
      "signumcongruentiae"))

  (define articulations
    '("accent"
      "espressivo"
      "marcato"
      "portato"
      "staccatissimo"
      "staccato"
      "tenuto"))

  (define ornaments
    '("prall"
      "mordent"
      "prallmordent"
      "turn"
      "upprall"
      "downprall"
      "upmordent"
      "downmordent"
      "lineprall"
      "prallprall"
      "pralldown"
      "prallup"
      "reverseturn"
      "trill"))

  (define instrument-specific
    '("upbow"
      "downbow"
      "flageolet"
      "thumb"
      "snappizzicato"
      "open"
      "halfopen"
      "stopped"
      "lheel"
      "rheel"
      "ltoe"
      "rtoe"))

  (define fermatas
    '("shortfermata"
      "fermata"
      "longfermata"
      "verylongfermata"))

  (define repeats
    '("segno"
      "coda"
      "varcoda"))


  ;; remove all remaining groups from the script-list
  (for-each
    (lambda (x) (set! script-list (delete x script-list)))
    (append ignore
            ancient
            articulations
            ornaments
            instrument-specific
            fermatas
            repeats))

  ;; require all scripts to appear here
  (if (pair? script-list) ; script-list should be empty by now
      (ly:error
        (_ "Unlisted scripts in Documentation/included/script-chart.ly: ~A")
        script-list))


  ;;;;;;; functions to generate the tables

  ;;; notes

  (define (make-script-note script)
    (make-event-chord
       (list (make-music
               'NoteEvent
               'duration
               (ly:make-duration 2 0 1 1)
               'pitch
               (ly:make-pitch 0 0 0))
             (make-music
               'ArticulationEvent
               'direction
               ;; everything goes up except "ictus"
               (if (string=? script "ictus") -1 1)
               'articulation-type
               script))))

  (define (make-script-notes scripts)
    (make-sequential-music
      (map make-script-note scripts)))

  (define (make-scripts-voice-context scripts)
    (let ((music (make-script-notes scripts)))
      (context-spec-music music 'Voice "voiceA")))

  (define (make-scripts-vaticana-context scripts)
    (let ((music (make-script-notes scripts)))
      (context-spec-music music 'VaticanaVoice "voiceA")))


  ;;; lyrics

  (define (make-script-lyric script)
    (make-event-chord
      (list (make-music
              'LyricEvent
              'duration
              (ly:make-duration 2 0 1 1)
              'text
              script))))

  (define (make-script-lyrics scripts)
    (make-sequential-music
      (map make-script-lyric scripts)))

  (define (make-scripts-lyrics-context scripts)
    (let ((music (make-script-lyrics scripts)))
      (context-spec-music music 'Lyrics "voiceA")))


  ;;; combining notes and lyrics

  (define (make-scripts-staff scripts)
    (make-simultaneous-music
      (list (make-scripts-voice-context scripts)
            (make-scripts-lyrics-context scripts))))

  (define (make-scripts-staff-ancient scripts)
    (make-simultaneous-music
      (list (make-scripts-vaticana-context scripts)
            (make-scripts-lyrics-context scripts))))

) % end of (begin ...)

\layout {
  line-width = 5.1\in
  indent = 0.0\mm
  \context {
    \Score
    timing = ##f
    barAlways = ##t
    \override NonMusicalPaperColumn.padding = #2.5
    \override PaperColumn.keep-inside-line = ##t
  }
  \context {
    \RhythmicStaff
    \remove "Time_signature_engraver"
    \override BarLine.transparent = ##t
    \override Stem.direction = #down
  }
  \context {
    \Lyrics
    \override LyricText.font-family = #'typewriter
    \override LyricText.font-shape = #'upright
  }
  \context {
    \VaticanaVoice
    \override Script.padding = #0
  }
}


scriptStaff =
#(define-music-function (parser location scripts) (list?)
   (make-scripts-staff scripts))

scriptStaffAncient =
#(define-music-function (parser location scripts) (list?)
   (make-scripts-staff-ancient scripts))
