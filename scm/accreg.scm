;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2013--2022 David Kastrup <dak@gnu.org>
;;;;
;;;; LilyPond is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LilyPond is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.


;; Accordion registration is tricky, partly because no two instruments
;; offer the same registers.  In particular bass registers are not
;; standardized at all and often left unspecified (orchestra scores
;; don't use bass notes anyway).
;;
;; registration is indicated by using a control sequence name
;; indicating the register set as either a markup function or a music
;; function, taking a string as argument.  The music function is a
;; standalone music event since register changes usually occur before
;; note onset.  It is currently implemented as a text superscript on
;; an empty chord but could conceivably become some kind of per-staff
;; rehearsal mark at one point of time.

(define-module (lily accreg))

(use-modules (lily) (srfi srfi-1) (ice-9 optargs))

;; It would be nice to generate the documentation string
;; automatically containing all possible registrations but this
;; is a hen-and-egg problem.  When the macro is being executed,
;; the register definition has not yet been evaluated.  It
;; would be feasible to not ever evaluate it and consider it
;; final.  But that seems like a somewhat unfriendly interface.
(define-syntax define-register-set
  (lambda (syntaks)
    "Defines markup command named with @var{set-symbol} for creating
accordion register markups as well as a music function of the same
name.

@var{doc} is the optional documentation string followed by the actual
@var{definition}.  See existing definitions in @file{scm/accreg.scm}
for examples."
    (syntax-case syntaks ()
      ((_ set-symbol doc definition)
       #`(begin
           (define-markup-command (set-symbol layout props name) (string?)
             #:properties (translate-scaled-markup)
             #:category accordion-registers
             doc
             (let* ((instrument definition)
                    (register
                     (ly:assoc-get name (ly:assoc-get 'register instrument)))
                    (reedbanks (ly:assoc-get 'reedbank instrument)))
               (interpret-markup
                layout props
                (make-general-align-markup
                 Y DOWN
                 (fold (lambda (d m)
                         (make-combine-markup
                          m (make-translate-scaled-markup
                             d (make-musicglyph-markup "accordion.dot"))))
                       (make-musicglyph-markup
                        (ly:assoc-get 'glyph instrument))
                       (or (ly:assoc-get 'dots register)
                           (append-map (lambda (x)
                                         (ly:assoc-get 'dots
                                                       (ly:assoc-get x reedbanks)))
                                       (ly:assoc-get 'reedbanks register))))))))

           (define-public set-symbol
             (define-music-function (register)
               (string?)
               #,(format #f "Equivalent to @code{<>^\\markup \\~a@var{REGISTER}}."
                         #'set-symbol)
               (make-event-chord
                (list
                 (make-music
                  'TextScriptEvent
                  'direction
                  1
                  'text
                  (#,(datum->syntax
                      syntaks
                      (string->symbol
                       (format #f "make-~a-markup"
                               (syntax->datum #'set-symbol))))
                   register))))))))
      ((_ set-symbol definition)
       #'(define-register-set set-symbol "Undocumented." definition)))))


(define-register-set discant
  "@code{\\discant @var{name}} generates a discant accordion register
symbol.

To make it available,
@example
#(use-modules (lily accreg))
@end example
is required near the top of your input file.

The register names in the default @code{\\discant} register set have
modeled after numeric Swiss notation like depicted in
@uref{http://de.wikipedia.org/wiki/Register_%28Akkordeon%29}, omitting
the slashes and dropping leading zeros.

The string @var{name} is basically a three-digit number with the
lowest digit specifying the number of 16' reeds, the tens the number
of 8' reeds, and the hundreds specifying the number of 4' reeds.
Without modification, the specified number of reeds in 8' is centered
in the symbol.  Newer instruments may have registrations where 8' can
be used either within or without a tone chamber, @q{cassotto}.
Notationally, the central dot then indicates use of cassotto.  One can
suffix the tens' digits @samp{1} and @samp{2} with @samp{+} or
@samp{-} to indicate clustering the dots at the right or left
respectively rather than centered.

Some examples are

@lilypond[quote]
#(use-modules (lily accreg))
\\markup {
  \\center-column {
     \\discant \"1\"
     \"\\\\discant \\\"1\\\"\"
     \\vspace #1
     \\discant \"120\"
     \"\\\\discant \\\"120\\\"\"
  } \\hspace #3
  \\center-column {
     \\discant \"1+0\"
     \"\\\\discant \\\"1+0\\\"\"
     \\vspace #1
     \\discant \"131\"
     \"\\\\discant \\\"131\\\"\"
  }
}
@end lilypond
"
  '((glyph . "accordion.discant")
    (reedbank
     (L (dots (0 . 0.5)))
     (M (dots (0 . 1.5)))
     (MM (dots (1 . 1.5)))
     (MMM (dots (-1 . 1.5)))
     (H (dots (0 . 2.5))))
    (register
     ("1" (reedbanks L))
     ("10" (reedbanks M))
     ("11" (reedbanks L M))
     ("1+0" (reedbanks MM))
     ("1+1" (reedbanks MM L))
     ("1-0" (reedbanks MMM))
     ("1-1" (reedbanks MMM L))
     ("20" (reedbanks MMM MM))
     ("21" (reedbanks MMM MM L))
     ("2+0" (reedbanks MM M))
     ("2+1" (reedbanks MM M L))
     ("2-0" (reedbanks MMM M))
     ("2-1" (reedbanks MMM M L))
     ("30" (reedbanks MMM MM M))
     ("31" (reedbanks MMM MM M L))
     ("100" (reedbanks H))
     ("101" (reedbanks H L))
     ("110" (reedbanks H M))
     ("111" (reedbanks H L M))
     ("11+0" (reedbanks H MM))
     ("11+1" (reedbanks H MM L))
     ("11-0" (reedbanks H MMM))
     ("11-1" (reedbanks H MMM L))
     ("120" (reedbanks H MMM MM))
     ("121" (reedbanks H MMM MM L))
     ("12+0" (reedbanks H MM M))
     ("12+1" (reedbanks H MM M L))
     ("12-0" (reedbanks H MMM M))
     ("12-1" (reedbanks H MMM M L))
     ("130" (reedbanks H MMM MM M))
     ("131" (reedbanks H MMM MM M L)))))

(define-register-set stdBass
  "@code{\\stdBass @var{name}} generates a standard bass accordion
register symbol.

To make it available,
@example
#(use-modules (lily accreg))
@end example
is required near the top of your input file.

The default bass register definitions have been modeled after the
article @uref{http://www.accordions.com/index/art/stradella.shtml}
originally appearing in Accord Magazine.

The underlying register model is

@lilypond[quote]
\\new PianoStaff
<<
  \\new Staff \\with { \\omit TimeSignature }
  { <c' c' c''>\\glissando <f' f' f''>
    <fis fis' fis''>\\glissando <b b'b''> }
  \\new Staff \\with { \\omit TimeSignature }
  { \\clef bass
    <c, c>\\glissando <f, f>
    <fis, fis>\\glissando <b, b>
  }
>>
@end lilypond

This kind of overlapping arrangement is common for Italian instruments
though the exact location of the octave breaks differ.

When not composing for a particular target instrument, using the five
reed definitions makes more sense than using a four reed layout: in
that manner, the @samp{Master} register is unambiguous.  This is
rather the rule in literature bothering about bass registrations at
all.

Available registrations are

@lilypond[quote]
#(use-modules (lily accreg))
\\markup {
  \\center-column {
     \\stdBass \"Soprano\"
     \"\\\\stdBass \\\"Soprano\\\"\"
     \\vspace #1
     \\stdBass \"Alto\"
     \"\\\\stdBass \\\"Alto\\\"\"
     \\vspace #1
     \\stdBass \"Tenor\"
     \"\\\\stdBass \\\"Tenor\\\"\"
     \\vspace #1
     \\stdBass \"Master\"
     \"\\\\stdBass \\\"Master\\\"\"
  } \\hspace #3
  \\center-column {
     \\stdBass \"Soft Bass\"
     \"\\\\stdBass \\\"Soft Bass\\\"\"
     \\vspace #1
     \\stdBass \"Soft Tenor\"
     \"\\\\stdBass \\\"Soft Tenor\\\"\"
     \\vspace #1
     \\stdBass \"Bass/Alto\"
     \"\\\\stdBass \\\"Bass/Alto\\\"\"
  }
}
@end lilypond
"
  '((glyph . "accordion.stdbass")
    (register
     ("Soprano" (reedbanks Soprano))
     ("Alto" (reedbanks Alto Soprano))
     ("Tenor" (reedbanks Tenor Alto Soprano))
     ("Master" (reedbanks Bass Tenor Contralto Alto Soprano))
     ("Soft Bass" (reedbanks Bass Tenor Contralto))
     ("Soft Tenor" (reedbanks Tenor Alto))
     ("Bass/Alto" (reedbanks Bass Alto Soprano)))
    (reedbank
     (Soprano (dots (0 . 3.5)))
     (Alto (dots (0 . 2.5)))
     (Contralto (dots (1 . 2)))
     (Tenor (dots (0 . 1.5)))
     (Bass (dots (0 . 0.5))))))


(define-register-set stdBassIV
  "@code{\\stdBassIV @var{name}} generates a standard bass accordion
register symbol.

To make it available,
@example
#(use-modules (lily accreg))
@end example
is required near the top of your input file.

The main use is for four-reed standard bass instruments with reedbank
layout

@lilypond[quote]
\\new PianoStaff
<<
  \\new Staff \\with { \\omit TimeSignature }
  { <e e'>2\\glissando <dis' dis''> }
  \\new Staff \\with { \\omit TimeSignature }
  { \\clef bass
    <e,, e,>\\glissando <dis, dis>
  }
>>
@end lilypond

Notable instruments are Morino models with MIII (the others are
five-reed instead) and the Atlantic@tie{}IV.  Most of those models
have three register switches.  Some newer Morinos with MIII might have
five or even seven.

The prevalent three-register layout uses the middle three switches
@samp{Tenor}, @samp{Master}, @samp{Soft Bass}.  Note that the sound is
quite darker than the same registrations of @samp{c,}-based
instruments.

Available registrations are

@lilypond[quote]
#(use-modules (lily accreg))
\\markup {
  \\center-column {
     \\stdBassIV \"Soprano\"
     \"\\\\stdBassIV \\\"Soprano\\\"\"
     \\vspace #1
     \\stdBassIV \"Alto\"
     \"\\\\stdBassIV \\\"Alto\\\"\"
     \\vspace #1
     \\stdBassIV \"Tenor\"
     \"\\\\stdBassIV \\\"Tenor\\\"\"
     \\vspace #1
     \\stdBassIV \"Master\"
     \"\\\\stdBassIV \\\"Master\\\"\"
  } \\hspace #3
  \\center-column {
     \\stdBassIV \"Soft Bass\"
     \"\\\\stdBassIV \\\"Soft Bass\\\"\"
     \\vspace #1
     \\stdBassIV \"Bass/Alto\"
     \"\\\\stdBassIV \\\"Bass/Alto\\\"\"
     \\vspace #1
     \\stdBassIV \"Soft Bass/Alto\"
     \"\\\\stdBassIV \\\"Soft Bass/Alto\\\"\"
     \\vspace #1
     \\stdBassIV \"Soft Tenor\"
     \"\\\\stdBassIV \\\"Soft Tenor\\\"\"
  }
}
@end lilypond
"
  '((glyph . "accordion.stdbass")
    (reedbank
     (Soprano (dots (0 . 3.5)))
     (Alto (dots (0 . 2.5)))
     (Tenor (dots (0 . 1.5)))
     (Bass (dots (0 . 0.5))))
    (register
     ("Soprano" (reedbanks Soprano))
     ("Alto" (reedbanks Alto Soprano))
     ("Tenor" (reedbanks Tenor Soprano))
     ("Master" (reedbanks Bass Tenor Alto Soprano))
     ("Soft Bass" (reedbanks Bass Tenor Alto))
     ("Bass/Alto" (reedbanks Bass Alto Soprano))
     ("Soft Bass/Alto" (reedbanks Bass Alto))
     ("Soft Tenor" (reedbanks Tenor Alto)))))

(define-register-set stdBassV
  "@code{\\stdBassV @var{name}} generates a standard bass accordion
register symbol.

To make it available,
@example
#(use-modules (lily accreg))
@end example
is required near the top of your input file.

The main use is for five-reed standard bass instruments with reedbank
layout

@lilypond[quote]
\\new PianoStaff
<<
  \\new Staff \\with { \\omit TimeSignature }
  { <e e' e''>2\\glissando <dis' dis'' dis'''> }
  \\new Staff \\with { \\omit TimeSignature }
  { \\clef bass
    <e,, e,>\\glissando <dis, dis>
  }
>>
@end lilypond

This tends to be the bass layout for Hohner's Morino series without
convertor or MIII manual.

With the exception of the rather new 7-register layout, the highest
two chord reeds are usually sounded together.  The  Older instruments offer
5 or 3 bass registers.  The Tango@tie{}VM offers an additional
@samp{Solo Bass} setting that mutes the chord reeds.  The symbol on
the register buttons of the Tango@tie{}VM would actually match the
physical five-octave layout reflected here, but it is not used in
literature.

Composers should likely prefer the five-reed versions of these
symbols.  The mismatch of a four-reed instrument with five-reed
symbols is easier to resolve for the player than the other way round.

Available registrations are

@lilypond[quote]
#(use-modules (lily accreg))
\\markuplist \\justified-lines {
  \\center-column {
     \\stdBassV \"Bass/Alto\"
     \"\\\\stdBassV \\\"Bass/Alto\\\"\"
     \\vspace #1
     \\stdBassV \"Soft Bass/Alto\"
     \"\\\\stdBassV \\\"Soft Bass/Alto\\\"\"
     \\vspace #1
     \\stdBassV \"Alto\"
     \"\\\\stdBassV \\\"Alto\\\"\"
     \\vspace #1
     \\stdBassV \"Tenor\"
     \"\\\\stdBassV \\\"Tenor\\\"\"
     \\vspace #1
     \\stdBassV \"Master\"
     \"\\\\stdBassV \\\"Master\\\"\"
 } \\hspace #3
  \\center-column {
     \\stdBassV \"Soft Bass\"
     \"\\\\stdBassV \\\"Soft Bass\\\"\"
     \\vspace #1
     \\stdBassV \"Soft Tenor\"
     \"\\\\stdBassV \\\"Soft Tenor\\\"\"
     \\vspace #1
     \\stdBassV \"Soprano\"
     \"\\\\stdBassV \\\"Soprano\\\"\"
     \\vspace #1
     \\stdBassV \"Sopranos\"
     \"\\\\stdBassV \\\"Sopranos\\\"\"
     \\vspace #1
     \\stdBassV \"Solo Bass\"
     \"\\\\stdBassV \\\"Solo Bass\\\"\"
  }
}
@end lilypond
"
  '((glyph . "accordion.stdbass")
    (reedbank
     (Sopranos (dots (-0.5 . 3.5) (0.5 . 3.5)))
     (Soprano (dots (0 . 3.5)))
     (Alto (dots (0 . 2.5)))
     (Tenor (dots (0 . 1.5)))
     (Bass (dots (0 . 0.5))))
    (register
     ("Bass/Alto" (reedbanks Bass Alto Soprano))
     ("Soft Bass/Alto" (reedbanks Bass Alto))
     ("Alto" (reedbanks Alto Sopranos))
     ("Tenor" (reedbanks Tenor Sopranos))
     ("Master" (reedbanks Bass Tenor Alto Sopranos))
     ("Soft Bass" (reedbanks Bass Tenor Alto))
     ("Soft Tenor" (reedbanks Tenor Alto))
     ("Soprano" (reedbanks Soprano))
     ("Sopranos" (reedbanks Sopranos))
     ("Solo Bass" (reedbanks Bass)))))

(define-register-set stdBassVI
  "@code{\\stdBassVI @var{name}} generates a standard bass accordion
register symbol for six reed basses.

To make it available,
@example
#(use-modules (lily accreg))
@end example
is required near the top of your input file.

This is primarily the register layout for the Hohner @qq{Gola} model.
The layout is

@lilypond[quote]
\\new PianoStaff
<<
  \\new Staff \\with { \\omit TimeSignature }
  { <c' c' c''>\\glissando <f' f' f''>
    <fis fis' fis''>\\glissando <b b'b''> }
  \\new Staff \\with { \\omit TimeSignature }
  { \\clef bass
    <c, c c'>\\glissando <f, f f'>
    <fis, fis fis>\\glissando <b, b b>
  }
>>
@end lilypond

The registers are effectively quite similar to that of
@code{\\stdBass}.  An additional bass reed at alto pitch is omitted
for esthetical reasons from the @samp{Master} setting, so the symbols
are almost the same except for the @samp{Alto/Soprano} register with
bass notes at Alto pitch and chords at Soprano pitch.

Available registrations are

@lilypond[quote]
#(use-modules (lily accreg))
\\markup {
  \\center-column {
     \\stdBassVI \"Soprano\"
     \"\\\\stdBassVI \\\"Soprano\\\"\"
     \\vspace #1
     \\stdBassVI \"Alto\"
     \"\\\\stdBassVI \\\"Alto\\\"\"
     \\vspace #1
     \\stdBassVI \"Soft Tenor\"
     \"\\\\stdBassVI \\\"Soft Tenor\\\"\"
     \\vspace #1
     \\stdBassVI \"Master\"
     \"\\\\stdBassVI \\\"Master\\\"\"
  } \\hspace #3
  \\center-column {
     \\stdBassVI \"Alto/Soprano\"
     \"\\\\stdBassVI \\\"Alto/Soprano\\\"\"
     \\vspace #1
     \\stdBassVI \"Bass/Alto\"
     \"\\\\stdBassVI \\\"Bass/Alto\\\"\"
     \\vspace #1
     \\stdBassVI \"Soft Bass\"
     \"\\\\stdBassVI \\\"Soft Bass\\\"\"
  }
}
@end lilypond
"
  '((glyph . "accordion.stdbass")
    (register
     ("Soprano" (reedbanks Soprano))
     ("Alto" (reedbanks Alto))
     ("Soft Tenor" (reedbanks Tenor Alto))
     ("Master" (reedbanks Bass Tenor Contralto Alto Soprano))
     ("Alto/Soprano" (reedbanks Contratenor Soprano))
     ("Bass/Alto" (reedbanks Bass Alto Soprano))
     ("Soft Bass" (reedbanks Bass Tenor Contralto)))
    (reedbank
     (Soprano (dots (0 . 3.5)))
     (Alto (dots (0 . 2.5)))
     (Contralto (dots (1 . 2)))
     (Contratenor (dots (-1 . 2.5)))
     (Tenor (dots (0 . 1.5)))
     (Bass (dots (0 . 0.5))))))

;; The default FreeBass is modeled after the default Discant register
;; description.  Being a default, we just provide the normal 2 reed
;; registrations.
(define-register-set freeBass
  "@code{\\freeBass @var{name}} generates a free bass/@/converter
accordion register symbol for the usual two-reed layout.

To make it available,
@example
#(use-modules (lily accreg))
@end example
is required near the top of your input file.

Available registrations are

@lilypond[quote]
#(use-modules (lily accreg))
\\markup {
  \\center-column {
     \\freeBass \"1\"
     \"\\\\freeBass \\\"1\\\"\"
     \\vspace #1
     \\freeBass \"10\"
     \"\\\\freeBass \\\"10\\\"\"
  } \\hspace #3
  \\center-column {
     \\freeBass \"11\"
     \"\\\\freeBass \\\"11\\\"\"
  }
}
@end lilypond
"
  '((glyph . "accordion.freebass")
    (reedbank
     (L (dots (0 . 0.5)))
     (M (dots (0 . 1.5))))
    (register
     ("1" (reedbanks L))
     ("10" (reedbanks M))
     ("11" (reedbanks L M)))))
