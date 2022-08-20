%% DO NOT EDIT this file manually; it was automatically
%% generated from the LilyPond Snippet Repository
%% (http://lsr.di.unimi.it).
%%
%% Make any changes in the LSR itself, or in
%% `Documentation/snippets/new/`, then run
%% `scripts/auxiliar/makelsr.pl`.
%%
%% This file is in the public domain.

\version "2.23.12"

\header {
  lsrtags = "editorial-annotations, scheme-language, tweaks-and-overrides"

  texidoc = "
To add a link to a grob stencil you can use @code{add-link} as defined
here. It works both with @code{\\override} and @code{\\tweak}.

Drawback: @code{point-and-click} is disturbed for the linked grobs.

Limitation: Works for PDF only.

The linked objects are colored with a separate command. Note that the
links are not displayed and are not clickable from inside the LSR.
"

  doctitle = "Adding links to objects"
} % begin verbatim


#(define (add-link url-strg)
  (lambda (grob)
    (let* ((stil (ly:grob-property grob 'stencil)))
      (if (ly:stencil? stil)
          (let* ((x-ext (ly:stencil-extent stil X))
                 (y-ext (ly:stencil-extent stil Y))
                 (url-expr `(url-link ,url-strg ,x-ext ,y-ext))
                 (new-stil
                   (ly:stencil-add
                     (ly:make-stencil url-expr x-ext y-ext)
                     stil)))
          (ly:grob-set-property! grob 'stencil new-stil))))))

%%%% test

%% For easier maintenance of this snippet the URL is formatted to use the
%% actually used LilyPond version.
%% Of course a literal URL would work as well.

#(define major.minor-version
  (string-join (take (string-split (lilypond-version) #\.) 2) "."))

urlI =
#(format #f
  "http://lilypond.org/doc/v~a/Documentation/notation/writing-pitches"
  major.minor-version)

urlII =
#(format #f
  "http://lilypond.org/doc/v~a/Documentation/notation/rhythms"
  major.minor-version)

urlIII =
#(format #f
  "http://lilypond.org/doc/v~a/Documentation/notation/note-heads"
  major.minor-version)

urlIV =
#(format #f
  "http://lilypond.org/doc/v~a/Documentation/notation/beams"
  major.minor-version)

urlV =
#(format #f
  "http://lilypond.org/doc/v~a/Documentation/notation/note-head-styles"
  major.minor-version)

urlVI =
#(format #f
  "http://lilypond.org/doc/v~a/Documentation/notation/writing-pitches"
  major.minor-version)

\relative c' {
   \key cis \minor

   \once \override Staff.Clef.color = #green
   \once \override Staff.Clef.after-line-breaking =
     #(add-link urlI)

   \once \override Staff.TimeSignature.color = #green
   \once \override Staff.TimeSignature.after-line-breaking =
     #(add-link urlII)

   \once \override NoteHead.color = #green
   \once \override NoteHead.after-line-breaking =
     #(add-link urlIII)

   cis'1
   \once \override Beam.color = #green
   \once \override Beam.after-line-breaking =
     #(add-link urlIV)
   cis8 dis e fis gis2
   <gis,
    \tweak Accidental.color #green
    \tweak Accidental.after-line-breaking #(add-link urlVI)
    \tweak color #green
    \tweak after-line-breaking #(add-link urlV)
    \tweak style #'harmonic
    bis
    dis
    fis
   >1
   <cis, cis' e>
}
