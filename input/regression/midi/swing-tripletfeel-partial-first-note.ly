\version "2.27.1"

\header {
  texidoc = "When @code{\\tripletFeel} is applied to music starting with
@code{\\partial 8}, the first note is on the off-beat grid position and must
be shortened to @code{8*2/3}.  Without the fix it would remain straight
(@code{8}), producing wrong swing timing."
}

#(ly:set-option 'warning-as-error #t)

\include "swing.ly"

%% Helper: return the duration (as a rational) of the first note or chord
%% in the top-level elements list of music (NoteEvent, EventChord, or
%% RestEvent), skipping directives such as ContextSpeccedMusic.
#(define (first-note-duration music)
   (let search ((elts (ly:music-property music 'elements)))
     (and (pair? elts)
          (let* ((e    (car elts))
                 (name (ly:music-property e 'name)))
            (if (or (eq? name 'NoteEvent)
                    (eq? name 'EventChord)
                    (eq? name 'RestEvent))
                (ly:moment-main (ly:music-length e))
                (search (cdr elts)))))))

musicWithPartialEighth = \tripletFeel 8 {
  \partial 8
  c'8 | c'8
}

%% Check the first note.
%% Expected: c'8 -> 8*2/3 = 1/12 (the note starts on the off-beat at 1/8).
#(let* ((dur (first-note-duration musicWithPartialEighth)))
   (if (not (= dur 1/12))
       (ly:error "First note after \\partial 8 has wrong duration: \
expected 1/12 (8*2/3), got ~a" dur)))

\score {
  \new Staff \tripletFeel 8 \musicWithPartialEighth
  \midi { }
}
