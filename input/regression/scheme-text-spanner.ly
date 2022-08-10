\version "2.23.6"

\header {
  texidoc = "Use @code{define-event-class}, scheme engraver methods,
and grob creation methods to create a fully functional text spanner
in scheme."
}

#(define-event-class 'scheme-text-span-event 'span-event)

#(define (add-grob-definition grob-entry)
   (set! all-grob-descriptions
         (cons
          ((@@ (lily) completize-grob-entry)
           grob-entry)
          all-grob-descriptions)))

#(add-grob-definition
  `(SchemeTextSpanner
    . ((bound-details . ((left . ((Y . 0)
                                 (padding . 0.25)
                                 (attach-dir . ,LEFT)
                                 ))
                        (left-broken . ((end-on-note . #t)))
                        (right . ((Y . 0)
                                  (padding . 0.25)
                                  ))
                       ))
       (dash-fraction . 0.2)
       (dash-period . 3.0)
       (direction . ,UP)
       (font-shape . italic)
       (left-bound-info . ,ly:horizontal-line-spanner::calc-left-bound-info)
       (outside-staff-priority . 350)
       (right-bound-info . ,ly:horizontal-line-spanner::calc-right-bound-info)
       (staff-padding . 0.8)
       (stencil . ,ly:line-spanner::print)
       (style . dashed-line)

       (meta . ((class . Spanner)
                (interfaces . (font-interface
                               horizontal-line-spanner-interface
                               line-interface
                               line-spanner-interface
                               outside-staff-interface
                               side-position-interface)))))))

#(define scheme-event-spanner-types
   '(
     (SchemeTextSpanEvent
      . ((description . "Used to signal where scheme text spanner brackets
start and stop.")
         (types . (post-event scheme-text-span-event span-event event))
         ))
     ))

#(set!
  scheme-event-spanner-types
  (map (lambda (x)
         (set-object-property! (car x)
                               'music-description
                               (cdr (assq 'description (cdr x))))
         (let ((lst (cdr x)))
           (set! lst (assoc-set! lst 'name (car x)))
           (set! lst (assq-remove! lst 'description))
           (hashq-set! music-name-to-property-table (car x) lst)
           (cons (car x) lst)))
       scheme-event-spanner-types))

#(set! music-descriptions
       (append scheme-event-spanner-types music-descriptions))

#(set! music-descriptions
       (sort music-descriptions alist<?))

#(define (add-bound-item spanner item)
   (if (null? (ly:spanner-bound spanner LEFT))
       (ly:spanner-set-bound! spanner LEFT item)
       (ly:spanner-set-bound! spanner RIGHT item)))

#(define (axis-offset-symbol axis)
   (if (eqv? axis X) 'X-offset 'Y-offset))

#(define (set-axis! grob axis)
  (if (not (number? (ly:grob-property grob 'side-axis)))
      (begin
        (set! (ly:grob-property grob 'side-axis) axis)
        (ly:grob-chain-callback
         grob
         (if (eqv? axis X)
             ly:side-position-interface::x-aligned-side
             side-position-interface::y-aligned-side)
         (axis-offset-symbol axis)))))

schemeTextSpannerEngraver =
#(lambda (context)
   (let ((span '())
         (finished '())
         (event-start '())
         (event-stop '()))
     (make-engraver
      (listeners ((scheme-text-span-event engraver event)
                  (if (= START (ly:event-property event 'span-direction))
                      (set! event-start event)
                      (set! event-stop event))))
      (acknowledgers ((note-column-interface engraver grob source-engraver)
                      (if (ly:spanner? span)
                          (begin
                            (ly:pointer-group-interface::add-grob span 'note-columns grob)
                            (add-bound-item span grob)))
                      (if (ly:spanner? finished)
                          (begin
                            (ly:pointer-group-interface::add-grob finished 'note-columns grob)
                            (add-bound-item finished grob)))))
      ((process-music trans)
       (if (ly:stream-event? event-stop)
           (if (null? span)
               (ly:warning "You're trying to end a scheme text spanner but you haven't started one.")
               (begin (set! finished span)
                      (ly:engraver-announce-end-grob trans finished event-start)
                      (set! span '())
                      (set! event-stop '()))))
       (if (ly:stream-event? event-start)
           (begin (set! span (ly:engraver-make-grob trans 'SchemeTextSpanner event-start))
                  (set-axis! span Y)
                  (set! event-start '()))))
      ((stop-translation-timestep trans)
       (if (and (ly:spanner? span)
                (null? (ly:spanner-bound span LEFT)))
           (ly:spanner-set-bound! span LEFT
             (ly:context-property context 'currentMusicalColumn)))
       (if (ly:spanner? finished)
           (begin
             (if (null? (ly:spanner-bound finished RIGHT))
                 (ly:spanner-set-bound! finished RIGHT
                   (ly:context-property context 'currentMusicalColumn)))
             (set! finished '())
             (set! event-start '())
             (set! event-stop '()))))
      ((finalize trans)
       (if (ly:spanner? finished)
           (begin
             (if (null? (ly:spanner-bound finished RIGHT))
                 (ly:spanner-set-bound! finished RIGHT
                   (ly:context-property context 'currentMusicalColumn)))
             (set! finished '())))
       (if (ly:spanner? span)
           (begin
             (ly:warning "I think there's a dangling scheme text spanner :-(")
             (ly:grob-suicide! span)
	     (set! span '())))))))

schemeTextSpannerStart =
#(make-span-event 'SchemeTextSpanEvent START)

schemeTextSpannerEnd =
#(make-span-event 'SchemeTextSpanEvent STOP)

\layout {
  \context {
    \Global
    \grobdescriptions #all-grob-descriptions
  }
  \context {
    \Voice
    \consists \schemeTextSpannerEngraver
  }
}

\relative {
  a4 b\schemeTextSpannerStart c d |
  \repeat unfold 20 { a4 b c d | }
  a4 b c\schemeTextSpannerEnd d |
  \override SchemeTextSpanner.to-barline = ##t
  a4\schemeTextSpannerStart b d c |
  \repeat unfold 20 { a4 b c d | }
  a1\schemeTextSpannerEnd |
}
