% Do not edit this file; it is automatically
% generated from Documentation/snippets/new
% This file is in the public domain.
%% Note: this file works from version 2.13.10
\version "2.13.18"

\header {
%% Translation of GIT committish: ee2fdacf2ff3acd7e6fb7c4005dfe698b1cc4eed

  texidoc = "

Se puede acceder @qq{lateralmente} a algunos grobs desde dentro de la
función de callback de otro grob.  Éstos se encuentran relacionados
normalmente como @qq{layout objects} (objetos de presentación) en la
sección @qq{Internal properties} (propiedades internas) de un
interface de grob.  Se usa la función @code{ly:grob-object} para
acceder a estos grobs.


Se presentan más abajo como ejemplo algunas formas de addecer a grobs
desde dentro de una función de callback de NoteHead, pero la técnica
no se limita a las cabezas de nota.  Sin embargo, la función de
callback de NoteHead es especialmente importante, porque es la función
de callback implícita que utiliza la instrucción @code{\\tweak}.


La función de ejemplo que se define abajo (\"display-grobs\") no es
probablemente tan útil, pero muestra que se está accediendo
efectivamente a los grobs.


Salida de ejemplo de la consola:


@example
--------------------
#-Grob Accidental -
#-Grob Arpeggio -
#-Grob Stem -
@end example


"

  doctitle = "Utilizar ly:grob-object para acceder a los grobs con \\tweak"


  lsrtags = "tweaks-and-overrides"

  texidoc = "
Some grobs can be accessed @qq{laterally} from within another grob's
callback.  These are usually listed as @qq{layout objects} in the
@qq{Internal properties} section of a grob-interface.  The function
@code{ly:grob-object} is used to access these grobs.


Demonstrated below are some ways of accessing grobs from within a
NoteHead callback, but the technique is not limited to NoteHeads.
However, the NoteHead callback is particularly important, since it is
the implicit callback used by the @code{\\tweak} command.


The example function defined below (\"display-grobs\") is probably not
that useful, but it demonstrates that the grobs are indeed being
accessed.


Example console output:


@example
--------------------
#-Grob Accidental -
#-Grob Arpeggio -
#-Grob Stem -
@end example


"
  doctitle = "Using ly:grob-object to access grobs with \\tweak"
} % begin verbatim


#(define (notehead-get-accidental notehead)
   ;; notehead is grob
   (ly:grob-object notehead 'accidental-grob))

#(define (notehead-get-arpeggio notehead)
   ;; notehead is grob
   (let ((notecolumn (notehead-get-notecolumn notehead)))
     (ly:grob-object notecolumn 'arpeggio)))

#(define (notehead-get-notecolumn notehead)
   ;; notehead is grob
   (ly:grob-parent notehead X))

#(define (notehead-get-stem notehead)
   ;; notehead is grob
   (let ((notecolumn (notehead-get-notecolumn notehead)))
     (ly:grob-object notecolumn 'stem)))

#(define (display-grobs notehead)
   ;; notehead is grob
   (let ((accidental (notehead-get-accidental notehead))
         (arpeggio (notehead-get-arpeggio notehead))
         (stem (notehead-get-stem notehead)))
     (format #t "~2&~a\n" (make-string 20 #\-))
     (for-each
      (lambda (x) (format #t "~a\n" x))
      (list accidental arpeggio stem))))

\relative c' {
  %% display grobs for each note head:
  %\override NoteHead #'before-line-breaking = #display-grobs
  <c
  %% or just for one:
  \tweak #'before-line-breaking #display-grobs
  es
  g>1\arpeggio
}
