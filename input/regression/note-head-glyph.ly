\version "2.27.1"

\header {
  texidoc = "The @code{glyph-name} property of noteheads contains an actual
glyph name in the font, which can also be overridden."
}

\layout {
  indent = 0
}

#(define (Text_script_to_notehead_engraver context)
   (make-engraver
    (acknowledgers
     ((note-head-interface engraver notehead source-engraver)
      (let
       ((script (ly:engraver-make-item engraver 'TextScript '())))
       (ly:grob-set-parent! script X notehead))))))

\layout {
  \context {
    \TabStaff
    \consists #Text_script_to_notehead_engraver
    \revert TextScript.stencil
  }
  \context {
    \Staff
    \consists #Text_script_to_notehead_engraver
    \consists Ambitus_engraver
  }
  \context {
    \MensuralStaff
    \consists #Text_script_to_notehead_engraver
  }
  \context {
    \Score
    \textLengthOn
    \override TextScript.font-size = -4
    \override TextScript.before-line-breaking =
    #(lambda (script)
       (define (format-string str)
         (if (equal? str "")
             "[empty]"
             str))
       (let ((notehead (ly:grob-parent script X)))
         (when (grob::has-interface notehead 'note-head-interface)
           (ly:grob-set-property!
            script
            'text
            (format-string (ly:grob-property notehead 'glyph-name))))))
  }
}

<<
  \new Staff \relative {
    c'1*1/4
    d2*1/2
    e4
    f4\harmonic
    |
    \approximatePitch a'4
    \approximatePitch a,,4
    \once \aikenHeads
    \pitchedTrill d'4\startTrillSpan cis
    a2*1/2 \stopTrillSpan
    |
    e2
    \tweak glyph-name "noteheads.u2arrow" e
  }
  \new TabStaff {
    c'4
    d
    \deadNote e
    \improvisationOn g
    |
  }
  \new MensuralStaff \relative {
    d'\longa*1/16
    e
    f\breve*1/8
    g
    |
  }
>>
