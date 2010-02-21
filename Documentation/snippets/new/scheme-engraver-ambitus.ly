\version "2.13.15"

\header {

  lsrtags = "contexts-and-engravers"
  
  
  texidoc = "This example demonstrates how the ambitus engraver may be
  defined on the user side, with a Scheme engraver.

  This is basically a rewrite in Scheme of the code from
  @file{lily/ambitus-engraver.cc}.
"

  doctitle = "Defining an engraver in Scheme: ambitus engraver"
}

#(use-modules (oop goops))

%%%
%%% Grob utilities
%%%
#(define (ly:event::in-event-class event class-name)
   (memq class-name (ly:make-event-class (ly:event-property event 'class))))

#(define (ly:separation-item::add-conditional-item grob grob-item)
   (ly:pointer-group-interface::add-grob grob 'conditional-elements grob-item))

#(define (ly:accidental-placement::accidental-pitch accidental-grob)
   (ly:event-property (ly:grob-property (ly:grob-parent accidental-grob Y) 'cause)
                      'pitch))

#(define (ly:accidental-placement::add-accidental grob accidental-grob)
   (let ((pitch (ly:accidental-placement::accidental-pitch accidental-grob)))
     (set! (ly:grob-parent accidental-grob X) grob)
     (set! (ly:grob-property accidental-grob 'X-offset)
           ly:grob::x-parent-positioning)
     (let* ((accidentals (ly:grob-object grob 'accidental-grobs))
            (handle (assq (ly:pitch-notename pitch) accidentals))
            (entry (if handle (cdr handle) '())))
       (set! (ly:grob-object grob 'accidental-grobs)
             (assq-set! accidentals (ly:pitch-notename pitch) (cons accidental-grob entry))))))

%%%
%%% Ambitus data structure
%%%
#(define-class <ambitus> ()
   (ambitus-line #:accessor ambitus-line)
   (ambitus-group #:accessor ambitus-group)
   (ambitus-up-note #:getter ambitus-up-note
                    #:init-form (make <ambitus-note>))
   (ambitus-down-note #:getter ambitus-down-note
                      #:init-form (make <ambitus-note>))
   (is-typeset #:accessor ambitus-is-typeset
               #:init-value #f)
   (start-c0 #:accessor ambitus-start-c0
             #:init-value #f)
   (start-key-sig #:accessor ambitus-start-key-sig
                  #:init-value '()))

#(define-method (ambitus-note (ambitus <ambitus>) direction)
   (if (= direction UP)
       (ambitus-up-note ambitus)
       (ambitus-down-note ambitus)))

#(define-accessor ambitus-head)
#(define-method (ambitus-head (ambitus <ambitus>) direction)
   (ambitus-note-head (ambitus-note ambitus direction)))
#(define-method ((setter ambitus-head) (ambitus <ambitus>) direction head)
   (set! (ambitus-note-head (ambitus-note ambitus direction)) head))

#(define-accessor ambitus-accidental)
#(define-method (ambitus-accidental (ambitus <ambitus>) direction)
   (ambitus-note-accidental (ambitus-note ambitus direction)))
#(define-method ((setter ambitus-accidental) (ambitus <ambitus>) direction accidental)
   (set! (ambitus-note-accidental (ambitus-note ambitus direction)) accidental))

#(define-accessor ambitus-cause)
#(define-method (ambitus-cause (ambitus <ambitus>) direction)
   (ambitus-note-cause (ambitus-note ambitus direction)))
#(define-method ((setter ambitus-cause) (ambitus <ambitus>) direction cause)
   (set! (ambitus-note-cause (ambitus-note ambitus direction)) cause))

#(define-accessor ambitus-pitch)
#(define-method (ambitus-pitch (ambitus <ambitus>) direction)
   (ambitus-note-pitch (ambitus-note ambitus direction)))
#(define-method ((setter ambitus-pitch) (ambitus <ambitus>) direction pitch)
   (set! (ambitus-note-pitch (ambitus-note ambitus direction)) pitch))

#(define-class <ambitus-note> ()
   (head #:accessor ambitus-note-head
         #:init-value #f)
   (accidental #:accessor ambitus-note-accidental
               #:init-value #f)
   (cause #:accessor ambitus-note-cause
          #:init-value #f)
   (pitch #:accessor ambitus-note-pitch
          #:init-value #f))

%%%
%%% Ambitus engraving logics
%%%
#(define (make-ambitus translator)
   (let ((ambitus (make <ambitus>)))
     (set! (ambitus-line ambitus) (ly:engraver-make-grob translator 'AmbitusLine '()))
     (set! (ambitus-group ambitus) (ly:engraver-make-grob translator 'Ambitus '()))
     (for-each (lambda (direction)
                 (let ((head (ly:engraver-make-grob translator 'AmbitusNoteHead '()))
                       (accidental (ly:engraver-make-grob translator 'AmbitusAccidental '()))
                       (group (ambitus-group ambitus)))
                   (set! (ly:grob-parent accidental Y) head)
                   (set! (ly:grob-object head 'accidental-grob) accidental)
                   (ly:axis-group-interface::add-element group head)
                   (ly:axis-group-interface::add-element group accidental)
                   (set! (ambitus-head ambitus direction) head)
                   (set! (ambitus-accidental ambitus direction) accidental)))
               (list DOWN UP))
     (set! (ly:grob-parent (ambitus-line ambitus) X) (ambitus-head ambitus DOWN))
     (ly:axis-group-interface::add-element (ambitus-group ambitus) (ambitus-line ambitus))
     (set! (ambitus-is-typeset ambitus) #f)
     ambitus))

#(define-method (typeset-ambitus (ambitus <ambitus>) translator)
   (if (not (ambitus-is-typeset ambitus))
       (begin
         (set! (ambitus-start-c0 ambitus)
               (ly:context-property (ly:translator-context translator)
                                    'middleCPosition
                                    0))
         (set! (ambitus-start-key-sig ambitus)
               (ly:context-property (ly:translator-context translator)
                                                              'keySignature))
         (set! (ambitus-is-typeset ambitus) #t))))

#(define-method (update-ambitus-notes (ambitus <ambitus>) note-grob)
   (let ((note-event (ly:grob-property note-grob 'cause)))
     (if (ly:event::in-event-class note-event 'note-event)
         (let ((pitch (ly:event-property note-event 'pitch)))
           (if (or (not (ambitus-pitch ambitus DOWN))
                   (ly:pitch<? pitch (ambitus-pitch ambitus DOWN)))
               (begin ;; update down pitch
                 (set! (ambitus-pitch ambitus DOWN) pitch)
                 (set! (ambitus-cause ambitus DOWN) note-event)))
           (if (or (not (ambitus-pitch ambitus UP))
                   (ly:pitch<? (ambitus-pitch ambitus UP) pitch))
               (begin ;; update up pitch
                 (set! (ambitus-pitch ambitus UP) pitch)
                 (set! (ambitus-cause ambitus UP) note-event)))))))

#(define-method (finalize-ambitus (ambitus <ambitus>) translator)
   (if (and (ambitus-pitch ambitus UP) (ambitus-pitch ambitus DOWN))
       (let ((accidental-placement (ly:engraver-make-grob translator
                                                          'AccidentalPlacement
                                                          (ambitus-accidental ambitus DOWN))))
         (for-each (lambda (direction)
                     (let ((pitch (ambitus-pitch ambitus direction)))
                       (set! (ly:grob-property (ambitus-head ambitus direction) 'cause)
                             (ambitus-cause ambitus direction))
                       (set! (ly:grob-property (ambitus-head ambitus direction) 'staff-position)
                             (+ (ambitus-start-c0 ambitus)
                                (ly:pitch-steps pitch)))
                       (let* ((handle (or (assoc (cons (ly:pitch-octave pitch)
                                                       (ly:pitch-notename pitch))
                                                 (ambitus-start-key-sig ambitus))
                                          (assoc (ly:pitch-notename pitch)
                                                 (ambitus-start-key-sig ambitus))))
                              (sig-alter (if handle (cdr handle) 0)))
                         (cond ((= (ly:pitch-alteration pitch) sig-alter)
                                (ly:grob-suicide! (ambitus-accidental ambitus direction))
                                (set! (ly:grob-object (ambitus-head ambitus direction)
                                                      'accidental-grob)
                                      '()))
                               (else
                                (set! (ly:grob-property (ambitus-accidental ambitus direction)
                                                        'alteration)
                                      (ly:pitch-alteration pitch)))))
                       (ly:separation-item::add-conditional-item (ambitus-head ambitus direction)
                                                                 accidental-placement)
                       (ly:accidental-placement::add-accidental accidental-placement
                                                                (ambitus-accidental ambitus direction))
                       (ly:pointer-group-interface::add-grob (ambitus-line ambitus)
                                                             'note-heads
                                                             (ambitus-head ambitus direction))))
                   (list DOWN UP))
         (ly:axis-group-interface::add-element (ambitus-group ambitus) accidental-placement))
       (begin ;; no pitch ==> suicide all grobs
         (for-each (lambda (direction)
                     (ly:grob-suicide! (ambitus-accidental ambitus direction))
                     (ly:grob-suicide! (ambitus-head ambitus direction)))
                   (list DOWN UP))
         (ly:grob-suicide! ambitus-line))))

%%%
%%% Ambitus engraver definition
%%%
#(define ambitus-engraver
   (lambda (context)
     (let ((ambitus #f))
       `((process-music . ,(lambda (translator)
                             (if (not ambitus)
                                 (set! ambitus (make-ambitus translator)))))
         (stop-translation-timestep . ,(lambda (translator)
                                         (if ambitus
                                             (typeset-ambitus ambitus translator))))
         (acknowledgers
          (note-head-interface . ,(lambda (engraver grob source-engraver)
                                    (if ambitus
                                        (update-ambitus-notes ambitus grob)))))
         (finalize . ,(lambda (translator)
                        (if ambitus
                            (finalize-ambitus ambitus translator))))))))

%%%
%%% Example
%%%

\score {
  \new StaffGroup <<
    \new Staff { c'4 des' e' fis' gis' }
    \new Staff { \clef "bass" c4 des ~ des ees b, }
  >>
  \layout { \context { \Staff \consists #ambitus-engraver } }
}
