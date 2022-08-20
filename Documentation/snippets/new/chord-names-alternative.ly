\version "2.23.6"

\header {
  lsrtags = "chords, legacy-code"

  texidoc = "
Chord names are generated from a list of pitches.  The functions which
construct these names can be customised.

Here are shown chords following Ignatzek (pp. 17-18, 1995), used by
default since LilyPond 1.7.20, compared with an alternative Jazz chord
notation and Harald Banter's (1987) notation.  A smaller font is used
in the latter case, as these tend to be overly verbose.

This mirrors the mechanism originally used in early LilyPond versions
(pre-1.7); not having been properly maintained, however, some features
have been lost (mainly chord exception lists) and bugs have been
introduced.
"

  doctitle = "Chord names alternative"
}


%%%% Legacy chord naming functions (formerly in scm/chord-generic-names.scm)
%%%% Copyright (C) 2003--2015 Jan Nieuwenhuizen <janneke@gnu.org>

#(set-global-staff-size 19.7)

#(define-public (banter-chordnames pitches bass inversion context)
  (old_chord->markup 'banter pitches bass inversion context))

#(define-public (jazz-chordnames pitches bass inversion context)
  (old_chord->markup 'jazz pitches bass inversion context))

#(define (define-translator-property symbol type? description)
  (if (not (and (symbol? symbol)
    (procedure? type?)
    (string? description)))
      (ly:error "error in call of define-translator-property"))
  (if (not (equal? (object-property symbol 'translation-doc) #f))
      (ly:error (G_ "symbol ~S redefined") symbol))

  (set-object-property! symbol 'translation-type? type?)
  (set-object-property! symbol 'translation-doc description)
  symbol)

#(for-each
  (lambda (x)
    (apply define-translator-property x))
  `((chordNameExceptionsFull ,list? "An alist of full chord
exceptions.  Contains @code{(@var{chord} . @var{markup})} entries.")
    (chordNameExceptionsPartial ,list? "An alist of partial chord
exceptions.  Contains @code{(@var{chord} . (@var{prefix-markup}
@var{suffix-markup}))} entries.")))

#(define-public (old_chord->markup
                style pitches bass inversion context)
  "Entry point for @code{Chord_name_engraver}.
@var{pitches}, @var{bass}, and @var{inversion} are lily pitches."
  (define (default-note-namer pitch)
    (note-name->markup pitch #f))

  (define (markup-or-empty-markup markup)
    "Return MARKUP if markup, else empty-markup"
    (if (markup? markup) markup empty-markup))

  (define (accidental->markup alteration)
    "Return accidental markup for ALTERATION."
    (if (= alteration 0)
        (make-line-markup (list empty-markup))
        (conditional-kern-before
         (alteration->text-accidental-markup alteration)
         (= alteration FLAT) 0.094725)))

  (define (list-minus a b)
    "Return list of elements in A that are not in B."
    (lset-difference eq? a b))

  (define (markup-join markups sep)
    "Return line-markup of MARKUPS, joining them with markup SEP"
    (if (pair? markups)
        (make-line-markup (list-insert-separator markups sep))
        empty-markup))

  (define (conditional-kern-before markup bool amount)
    "Add AMOUNT of space before MARKUP if BOOL is true."
    (if bool
        (make-line-markup
         (list (make-hspace-markup amount)
           markup))
        markup))

  (define (step-nr pitch)
    (let* ((pitch-nr (+ (* 7 (ly:pitch-octave pitch))
                        (ly:pitch-notename pitch)))
           (root-nr (+ (* 7 (ly:pitch-octave (car pitches)))
                       (ly:pitch-notename (car pitches)))))
      (+ 1 (- pitch-nr root-nr))))

  (define (next-third pitch)
    (ly:pitch-transpose pitch
                        (ly:make-pitch 0 2 (if (or (= (step-nr pitch) 3)
                                                   (= (step-nr pitch) 5))
                                               FLAT 0))))

  (define (step-alteration pitch)
    (let* ((diff (ly:pitch-diff (ly:make-pitch 0 0 0) (car pitches)))
           (normalized-pitch (ly:pitch-transpose pitch diff))
           (alteration (ly:pitch-alteration normalized-pitch)))
      (if (= (step-nr pitch) 7) (+ alteration SEMI-TONE) alteration)))

  (define (pitch-unalter pitch)
    (let ((alteration (step-alteration pitch)))
      (if (= alteration 0)
          pitch
          (ly:make-pitch (ly:pitch-octave pitch) (ly:pitch-notename pitch)
                         (- (ly:pitch-alteration pitch) alteration)))))

  (define (step-even-or-altered? pitch)
    (let ((nr (step-nr pitch)))
      (if (!= (modulo nr 2) 0)
          (!= (step-alteration pitch) 0)
          #t)))

  (define (step->markup-plusminus pitch)
    (let ((alt (step-alteration pitch)))
      (make-line-markup
       (list
        (number->string (step-nr pitch))
        (cond
         ((= alt DOUBLE-FLAT) "--")
         ((= alt FLAT) "-")
         ((= alt NATURAL) "")
         ((= alt SHARP) "+")
         ((= alt DOUBLE-SHARP) "++"))))))

  (define (step->markup-accidental pitch)
    (make-line-markup
     (list (accidental->markup (step-alteration pitch))
           (make-simple-markup (number->string (step-nr pitch))))))

  (define (step->markup-ignatzek pitch)
    (make-line-markup
     (if (and (= (step-nr pitch) 7)
              (= (step-alteration pitch) 1))
         (list (ly:context-property context 'majorSevenSymbol))
         (list (accidental->markup (step-alteration pitch))
               (make-simple-markup (number->string (step-nr pitch)))))))

  ;; tja, kennok
  (define (make-sub->markup step->markup)
    (lambda (pitch)
      (make-line-markup (list (make-simple-markup "no")
                              (step->markup pitch)))))

  (define (step-based-sub->markup step->markup pitch)
    (make-line-markup (list (make-simple-markup "no") (step->markup pitch))))

  (define (get-full-list pitch)
    (if (<= (step-nr pitch) (step-nr (last pitches)))
        (cons pitch (get-full-list (next-third pitch)))
        '()))

  (define (get-consecutive nr pitches)
    (if (pair? pitches)
        (let* ((pitch-nr (step-nr (car pitches)))
               (next-nr (if (!= (modulo pitch-nr 2) 0) (+ pitch-nr 2) nr)))
          (if (<= pitch-nr nr)
              (cons (car pitches) (get-consecutive next-nr (cdr pitches)))
              '()))
        '()))

  ;;; FIXME -- exceptions no longer work. -vv

  (define (full-match exceptions)
    (if (pair? exceptions)
        (let* ((e (car exceptions))
               (e-pitches (car e)))
          (if (equal? e-pitches pitches)
              e
              (full-match (cdr exceptions))))
        #f))

  (define (partial-match exceptions)
    (if (pair? exceptions)
        (let* ((e (car exceptions))
               (e-pitches (car e)))
          (if (equal? e-pitches (take pitches (length e-pitches)))
              e
              (partial-match (cdr exceptions))))
        #f))

  ;; FIXME: exceptions don’t work anyway.
  (if #f (begin
           (write-me "pitches: " pitches)))
  (let* ((full-exceptions
          (ly:context-property context 'chordNameExceptionsFull))
         (full-exception (full-match full-exceptions))
         (full-markup (if full-exception (cadr full-exception) '()))
         (partial-exceptions
          (ly:context-property context 'chordNameExceptionsPartial))
         (partial-exception (partial-match partial-exceptions))
         (partial-pitches (if partial-exception (car partial-exception) '()))
         (partial-markup-prefix
          (if partial-exception (markup-or-empty-markup
                                 (cadr partial-exception)) empty-markup))
         (partial-markup-suffix
          (if (and partial-exception (pair? (cddr partial-exception)))
              (markup-or-empty-markup (caddr partial-exception)) empty-markup))
         (root (car pitches))
         (full (get-full-list root))
         ;; kludge alert: replace partial matched lower part of all with
         ;; 'normal' pitches from full
         ;; (all pitches)
         (all (append (take full (length partial-pitches))
                      (drop pitches (length partial-pitches))))

         (highest (last all))
         (missing (list-minus full (map pitch-unalter all)))
         (consecutive (get-consecutive 1 all))
         (rest (list-minus all consecutive))
         (altered (filter step-even-or-altered? all))
         (cons-alt (filter step-even-or-altered? consecutive))
         (base (list-minus consecutive altered)))


    (if #f (begin
             (write-me "full:" full)
             ;; (write-me "partial-pitches:" partial-pitches)
             (write-me "full-markup:" full-markup)
             (write-me "partial-markup-perfix:" partial-markup-prefix)
             (write-me "partial-markup-suffix:" partial-markup-suffix)
             (write-me "all:" all)
             (write-me "altered:" altered)
             (write-me "missing:" missing)
             (write-me "consecutive:" consecutive)
             (write-me "rest:" rest)
             (write-me "base:" base)))

    (case style
      ((banter)
       ;;    root
       ;;    + steps:altered + (highest all -- if not altered)
       ;;    + subs:missing

       (let* ((root->markup default-note-namer)
              (step->markup step->markup-plusminus)
              (sub->markup (lambda (x)
                              (step-based-sub->markup step->markup x)))
              (sep (make-simple-markup "/")))

         (if
          (pair? full-markup)
          (make-line-markup (list (root->markup root) full-markup))

          (make-line-markup
           (list
            (root->markup root)
            partial-markup-prefix
            (make-super-markup
             (markup-join
              (append
               (map step->markup
                    (append altered
                            (if (and (> (step-nr highest) 5)
                                     (not
                                      (step-even-or-altered? highest)))
                                (list highest) '())))
               (list partial-markup-suffix)
               (map sub->markup missing))
              sep)))))))


      ((jazz)
       ;;    root
       ;;    + steps:(highest base) + cons-alt
       ;;    + 'add'
       ;;    + steps:rest
       (let* ((root->markup default-note-namer)
              (step->markup step->markup-ignatzek)
              (sep (make-simple-markup " "))
              (add-prefix (make-simple-markup " add")))

         (if
          (pair? full-markup)
          (make-line-markup (list (root->markup root) full-markup))

          (make-line-markup
           (list
            (root->markup root)
            partial-markup-prefix
            (make-super-markup
             (make-line-markup
              (list

               ;; kludge alert: omit <= 5
               ;;(markup-join (map step->markup
               ;;                        (cons (last base) cons-alt)) sep)

               ;; This fixes:
               ;;  c     C5       -> C
               ;;  c:2   C5 2     -> C2
               ;;  c:3-  Cm5      -> Cm
               ;;  c:6.9 C5 6add9 -> C6 add 9 (add?)
               ;;  ch = \chords { c c:2 c:3- c:6.9^7 }
               (markup-join (map step->markup
                                 (let ((tb (last base)))
                                   (if (> (step-nr tb) 5)
                                       (cons tb cons-alt)
                                       cons-alt))) sep)

               (if (pair? rest)
                   add-prefix
                   empty-markup)
               (markup-join (map step->markup rest) sep)
               partial-markup-suffix))))))))

      (else empty-markup))))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Here begins the actual snippet:

chs = \transpose c' c' {
  <c e g>1
  <c es g>  % m = minor triad
  <c e gis>
  <c es ges> \break
  <c e g bes>
  <c es g bes>
  <c e g b>  % triangle = maj
  <c es ges beses>
  <c es ges b> \break
  <c e gis bes>
  <c es g b>
  <c e gis b>
  <c es ges bes> \break
  <c e g a>  % 6 = major triad with added sixth
  <c es g a>  % m6 = minor triad with added sixth
  <c e g bes d'>
  <c es g bes d'> \break
  <c es g bes d' f' a' >
  <c es g bes d' f' >
  <c es ges bes d' >
  <c e g bes des' > \break
  <c e g bes dis'>
  <c e g bes d' f'>
  <c e g bes d' fis'>
  <c e g bes d' f' a'> \break
  <c e g bes d' fis' as'>
  <c e gis bes dis'>
  <c e g bes dis' fis'>
  <c e g bes d' f' as'> \break
  <c e g bes des' f' as'>
  <c e g bes d' fis'>
  <c e g b d'>
  <c e g bes d' f' as'> \break
  <c e g bes des' f' as'>
  <c e g bes des' f' a'>
  <c e g b d'>
  <c e g b d' f' a'> \break
  <c e g b d' fis'>
  <c e g bes des' f ' a'>
  <c f g>
  <c f g bes> \break
  <c f g bes d'>
  <c e g d'>  % add9
  <c es g f'>
  <c e g b fis'>  % Lydian
  <c e g bes des' ees' fis' aes'>  % altered chord
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% alternate Jazz notation

efullmusicJazzAlt = {
  <c e gis>1-\markup { "+" }
  <c e g b>-\markup {
    \normal-size-super
    % \override #'(font-family . math) "N"
    \override #'(font-family . math) "M"
  }
  %%c:3.5.7 = \markup { \override #'(font-family . math) "M" }
  %%c:3.5.7 = \markup { \normal-size-super "maj7" }

  <c es ges>-\markup { \super "o" }  % should be $\circ$ ?
  <c es ges bes>-\markup { \super \combine "o" "/" }
  <c es ges beses>-\markup { \super  "o7" }
}

efullJazzAlt = #(sequential-music-to-chord-exceptions efullmusicJazzAlt #f)

epartialmusicJazzAlt = {
  <c d>1-\markup { \normal-size-super "2" }
  <c es>-\markup { "m" }
  <c f>-\markup { \normal-size-super "sus4" }
  <c g>-\markup { \normal-size-super "5" }
  %% TODO, partial exceptions
  <c es f>-\markup { "m" }-\markup { \normal-size-super "sus4" }
  <c d es>-\markup { "m" }-\markup { \normal-size-super "sus2" }
}

epartialJazzAlt = #(sequential-music-to-chord-exceptions epartialmusicJazzAlt #f)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\score {
  <<
    \new ChordNames {
      %% Already set by default:
      %\set chordNameFunction = #ignatzek-chord-names
      \set instrumentName = "Ignatzek"
      \set shortInstrumentName = "Def"
      \chs
    }

    \new ChordNames {
      \set chordNameFunction = #jazz-chordnames
      \set majorSevenSymbol = \whiteTriangleMarkup
      \set chordNameSeparator = "/"
      \set chordNameExceptionsFull = \efullJazzAlt
      \set chordNameExceptionsPartial = \epartialJazzAlt
      \set instrumentName = "Alternative"
      \set shortInstrumentName = "Alt"
      \chs
    }

    %% This is the Banter (1987) style.  It gives exceedingly
    %% verbose (wide) names, making the output file take up to 4 pages.

    \new ChordNames {
      \set chordNameFunction = #banter-chordnames
      \override ChordName.font-size = #-3
      \set instrumentName = "Banter"
      \set shortInstrumentName = "Ban"
      \chs
    }

  \new Staff \transpose c c' { \chs }
  >>
  \layout {
    #(layout-set-staff-size 16)
    system-system-spacing.basic-distance = #0
    \context {
      \ChordNames
      \consists "Instrument_name_engraver"
    }
    \context {
      \Score
      \remove "Bar_number_engraver"
    }
  }
}
