%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2013--2023 Harm <thomasmorley65@gmail.com>
%%%%
%%%% LilyPond is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%%
%%%% LilyPond is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%%
%%%% You should have received a copy of the GNU General Public License
%%%% along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

\version "2.25.9"

#(define (add-dot text?)
   (lambda (grob)
     (let* ((layout (ly:grob-layout grob))
            (props (layout-extract-page-properties layout))
            (font
             (ly:paper-get-font layout
                                (cons '((font-encoding . fetaMusic)) props)))
            ;; Get the stencil procedure from ly:grob-basic-properties.
            ;; If any, use it to create the stencil.
            (function (assoc-get 'stencil (ly:grob-basic-properties grob)))
            (stencil (if function (function grob) point-stencil))
            ;; Get the grob name and create a text stencil.
            ;; Read out the y-length for later translation.
            (grob-name (grob::name grob))
            (grob-string (if (symbol? grob-name)
                             (symbol->string grob-name)
                             "no name"))
            (ref-text-stil
             (grob-interpret-markup grob
                                    (markup
                                     #:with-color red
                                     #:normal-text
                                     #:abs-fontsize 6
                                     (string-append "   " grob-string))))
            (ref-text-stil-length
             (interval-length (ly:stencil-extent ref-text-stil Y)))
            (grob-string-stil
             (if text?
                 (grob-interpret-markup grob
                                        (markup
                                         #:with-dimensions '(0 . 0) '(0 . 0)
                                         #:stencil ref-text-stil))
                 point-stencil))
            ;; Create a red-dot stencil
            (dot (ly:font-get-glyph font "dots.dot"))
            (red-dot (ly:stencil-in-color dot 1 0 0))
            (red-dot-length (interval-length (ly:stencil-extent red-dot X)))
            (red-dot-stil
             (ly:stencil-translate-axis red-dot (/ red-dot-length -2) X)))
       ;; If there's a grob with a stencil -procedure and a valid stencil is
       ;; created, add red-dot-stil and an optional text stencil.
       (if (and function (ly:stencil? stencil) (grob::is-live? grob))
           (ly:grob-set-property!
            grob 'stencil
            (ly:stencil-add stencil
                            red-dot-stil
                            (if text?
                                (ly:stencil-translate-axis
                                 (ly:stencil-rotate grob-string-stil 45 0 0)
                                 (/ ref-text-stil-length 2)
                                 X)
                                point-stencil)))))))

#(define (add-red-dot-to-grobs text? l)
   ;; possible values for l:
   ;;   'all-grobs (adds red-dots to all grobs, where possible)
   ;;          this will naturally cause collisions,
   ;;   a single grob-name, must be a symbol,
   ;;   a list of grob-names,
   ;;   anything else (returns the unchanged original stencil)
   ;;  TODO: How to apply it once?
   (let ((grobs-to-consider
          (cond ((eq? l 'all-grobs)
                 all-grob-descriptions)
                ((symbol? l)
                 (list (assoc l all-grob-descriptions)))
                ((list? l)
                 (map (lambda (grob)
                        (assoc grob all-grob-descriptions))
                      l))
                (else '()))))
     (lambda (context)
       (let loop ((x grobs-to-consider))
         (if (not (null? x))
             (let ((grob-name (caar x)))
               (ly:context-pushpop-property context
                                            grob-name
                                            'after-line-breaking
                                            (add-dot text?))
               (loop (cdr x))))))))


printRefpoint =
#(define-music-function
   (text? s-or-l)
   (boolean? symbol-list-or-symbol?)
   "Mark grob reference points in a context with red dots.

If @var{text?} is set to @code{##t}, add labels to the red dots,
showing the corresponding grob name.

This is a context modifier function.

Possible values for @var{s-or-l} are the symbol @code{'all-grobs} to
indicate that red dots should be added to all grobs where possible
(this naturally causes collisions), a single grob-name (must be a
symbol), or a list of grob names.

To avoid bleeding-overs, any context has to be initiated explicitly."
#{
  \applyContext #(add-red-dot-to-grobs text? s-or-l)
#})


%% For single use:

#(define addDot
   (lambda (grob)
     (let* ((function (assoc-get 'stencil (ly:grob-basic-properties grob)))
            (stencil (if function (function grob) point-stencil))
            (layout (ly:grob-layout grob))
            (props (layout-extract-page-properties layout))
            (font
             (ly:paper-get-font layout
                                (cons '((font-encoding . fetaMusic)) props)))
            (dot (ly:font-get-glyph font "dots.dot"))
            (red-dot (ly:stencil-in-color dot 1 0 0))
            (red-dot-length (interval-length (ly:stencil-extent red-dot X)))
            (red-dot-stil
             (ly:stencil-translate-axis red-dot (/ red-dot-length -2) X)))
       (if (and function (ly:stencil? stencil) (grob::is-live? grob))
           (ly:grob-set-property! grob 'stencil
                                  (ly:stencil-add
                                   stencil
                                   red-dot-stil))))))


%% Overriding grobs must be defined separately.
%% Don't forget to specify the context if necessary.
onceRedScript = \once \override Script.after-line-breaking = #addDot


%{

%%%%%%%%%%%%
%%% EXAMPLES
%%%%%%%%%%%%

mus =
{
  \override NoteHead.style = #'altdefault
  g'2->
  % Testing whether \printRefpoint works with a custom override.
  \once \override Script.stencil =
  #(lambda (grob)
    (ly:font-get-glyph (ly:grob-default-font grob) "scripts.coda"))

  c''\fermata |
  as'1^"Yogi" |
  b'\breve _"Larry" |
  \mark "Twinkle" e''8 s4.
  \bar "|."
}


\markup { red dots for \concat { \typewriter TimeSignature , }
	  \concat { \typewriter Script , } and \typewriter BarLine }
\new Staff \with { \printRefpoint ##f #'(TimeSignature Script BarLine) } \mus

\markup { red dots for \concat { \typewriter TextScript , }
	  plus additional text }
\new Staff \with { \printRefpoint ##t #'TextScript } \mus

\markup { red dots for all grobs }
\new Staff \with { \printRefpoint ##f #'all-grobs } \mus

\markup { red dots for all grobs, plus additional text }
\new Staff \with { \printRefpoint ##t #'all-grobs } \mus

\markup { red dot once for \typewriter Script using
	  \typewriter "\onceRedScript" }
{
  \override NoteHead.style = #'altdefault
  g'2->
  \onceRedScript
  % Testing whether \printRefpoint works with a custom-override.
  \once \override Script.stencil =
  #(lambda (grob)
    (ly:font-get-glyph (ly:grob-default-font grob) "scripts.coda"))
  c''\fermata |
  as'1^"Yogi" |
  b'\breve _"Larry" |
  \mark "Twinkle" e''8-! s4.
  \bar "|."
}

\layout {
  \context {
    \Score
    \remove "Mark_engraver"
  }
  \context {
    \Staff
    \consists "Mark_engraver"
  }
}

%}
