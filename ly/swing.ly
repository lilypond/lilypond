%%% -*- Mode: Scheme -*-
%
% Copyright (C) 2012--2022 Johannes Rohrer <src@johannesrohrer.de>
%
% This program is free software: you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
% General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see
% <http://www.gnu.org/licenses/>.

%% Provide music functions that scale music event durations to create
%% a swing feeling.  This influences both notation spacing and MIDI output.

%%   \applySwing swingDiv weightList music
%%
%%   \applySwingWithOffset swingDiv offset weightList music
%%
%%   \tripletFeel swingDiv music
%%
%% See the function docstrings and usage examples below for more
%% information.

%% Originally inspired by contributions to LilyPond issue #687
%% <https://sourceforge.net/p/testlilyissues/issues/687>,
%% especially by Dean Radcliffe <chicagogrooves_at_gmail_dot_com>
%% and Arvid Grøtting <arvidgr_at_gmail_dot_com>.


%%% Usage examples:

%%   %% swung eigth or sixteenth notes
%%
%%   \tripletFeel 8 { c8 c }
%%   %% result: { c8*4/3 c8*2/3 },
%%   %% corresponding to \times 2/3 { c4 c8 }
%%
%%   \tripletFeel 16 { c16 c }
%%   %% result: { c16*4/3 c16*2/3 },
%%   %% corresponding to \times 2/3 { c8 c16 }
%%
%%   %% samba swing
%%
%%   \applySwing 16 #'(3 2 2 3) { c16 c c c }
%%   %% result corresponding to \times 4/5 { c16. c16 c c16. }
%%
%%   \applySwing 16 #'(4 3 3 4) { c16 c c c } % smoother
%%   %% result corresponding to \times 8/7 { c16 c32. c c16 }


%%% Known limitations:

%% * \repeat constructs in music (even \repeat unfold) are not taken
%%   into consideration when determining note timing. This will lead
%%   to problems unless the durations of all repeated parts are
%%   integer multiples of the swing cycle duration.
%%
%% * These functions are oblivious to time signatures and measures.
%%
%%   That is why offsets need to be supplied by the user if music
%%   starts off-beat.
%%
%% * Grace notes are ignored and simply left unaffected.
%%
%% * Treatment of notes shorter than swingDiv is ad-hoc and may not be
%%   what you want.
%%
%%   In some detail: \applySwing operates by shifting the boundary
%%   between two events if it falls on an off-beat multiple of
%%   swingDiv. Assuming forward shift (backward shift works
%%   analogously), this is done by first lengthening the earlier
%%   event, then shortening the following event(s) by the same amount.
%%
%%   Notes shorter than swingDiv never trigger the first step, hence
%%   all notes in the following examples remain straight:
%%
%%      \tripletFeel 8 { c16 c c c }
%%      \tripletFeel 8 { c16 c c8 }
%%
%%   However, if they appear after a note that has already been
%%   lengthened, they will be shortened in the second step. Therefore,
%%
%%      \tripletFeel 8 { c8 c16 c }
%%
%%   will result in timing corresponding to \times 2/3 { c4 c16 c }.

\version "2.21.0"

%%% Helper functions

#(define (moment-abs mom)
   (if (ly:moment<? mom ZERO-MOMENT)
       (ly:moment-sub ZERO-MOMENT mom)
       mom))

#(define (chord-or-with-duration? music)
   (or
    (eq? (ly:music-property music 'name) 'EventChord)
    (ly:duration? (ly:music-property music 'duration))))

#(define (music-lengthen music add-moment)
   "Uniformly scale durations in @var{music} such that it takes
@var{add-moment} more time in total."
   (let* ((dur (ly:moment-main (ly:music-length music)))
          (delta (ly:moment-main add-moment))
          (factr (ly:make-moment (1+ (/ delta dur)))))
     (ly:music-compress music factr)))

#(define* (map-events-with-timing
           function
           music
           #:optional (start-time ZERO-MOMENT))
   "Walk through @var{music} and transform each chord or event with
duration (e.g. NoteEvent) @code{evt} to @code{(@var{function} evt
time)}. Here @code{time} provides the moment at which @code{evt}
occurs, with @var{start-time} assigned to the earliest such event
encountered in @var{music}.

Ignore grace music. Do not unfold repeats for determining timings."
   (map-some-music
    (lambda (mus)
      (cond
       ((eq? (ly:music-property mus 'name) 'GraceMusic)
        ;; ignore grace notes (leave mus unchanged and stop recursing)
        mus)
       ((eq? (ly:music-property mus 'name) 'SimultaneousMusic)
        ;; recursively call this function on all parallel music elements
        (let ((es (ly:music-property mus 'elements))
              (e (ly:music-property mus 'element)))
          (if (pair? es)
              (set! (ly:music-property mus 'elements)
                    (map (lambda (x)
                           (map-events-with-timing function x start-time))
                      es)))
          ;; Not sure if this can actually happen with SimultaneousMusic?
          ;; Anyway, better safe than sorry.
          (if (ly:music? e)
              (set! (ly:music-property mus 'element)
                    (map-events-with-timing function e start-time)))
          ;; advance time counter by total length of simultaneous music
          (set! start-time
                (ly:moment-add start-time (ly:music-length mus)))
          mus))
       ((chord-or-with-duration? mus)
        (let ((evt-time start-time))
          ;; advance time counter
          (set! start-time
                (ly:moment-add start-time (ly:music-length mus)))
          (function mus evt-time)))
       (else
        ;; if we did not handle mus above, keep recursing
        #f)))
    music))

%%% Grid calculations

%% Assume we want to swing notes of duration s in cycles of n notes,
%% such that the i-th note in such a group gets a relative length of
%% wi, but the length u = n*s of the cycle as a whole remains
%% unchanged. This corresponds to a function call like
%%
%%   \applySwing s '(w1 w2 ... wn) music
%%
%% and means that we want to distort a "straight" temporal grid
%%
%%   0       s      2s       (n-1)s       u     u+s ...
%%   |  (s)  :  (s)  :     ...    :  (s)  |  (s)  : ... (periodically)
%%
%% to something like
%%
%%   0       s+d1  2s+d2    (n-1)s+d(n-1) u   ...
%%   | (w1)    : (w2) :    ...     : (wn) |   ...       (periodically)
%%
%% We can calculate the deltas di from the weights wi as
%%
%%   di = d(i-1) + (wi/W)*u - s
%%
%% with W = w1 + w2 + ... + wn. The function
%% calculate-normalized-deltas does this for the special case u = 1,
%% i.e. s = 1/n, working with rational numbers; calculate-delta-alist
%% uses this to calculate an alist of lilypond moments, associating
%% grid line positions s, 2s, ... i*s, ... with the corresponding
%% deltas d1, d2, ... di, ...

#(define (calculate-normalized-deltas wlist)
   "Based on a subdivision of the unit interval [0;1] into n =
length(wlist) sub-intervals of equal width 1/n, calculate the
necessary shifts of the sub-interval boundaries to make the relative
lengths of the sub-intervals correspond to the ratios of the numbers
in wlist.

Return a list of n-1 numbers."
   (let* ((n (length wlist))
          (wsum (apply + wlist))
          (div-dur-adjustments (map (lambda (w) (- (/ w wsum) (/ 1 n)))
                                 wlist)))
     (let to-deltas ((adjs div-dur-adjustments)
                     (prev-delta 0))
       (if (or (null? adjs) (null? (cdr adjs)))
           '()
           (let ((next-delta (+ prev-delta (car adjs))))
             (cons next-delta (to-deltas (cdr adjs) next-delta)))))))

#(define (calculate-delta-alist wlist swing-div)
   "Based on a pulse of n = length(wlist) notes of original duration
swing-div (a moment), calculate the necessary shifts of the note
boundaries (as moments, again) to make the relative length of the
notes correspond to the ratios of the numbers in wlist.

Return an alist associating old boundary positions with required
shifts, both given as moments."
   (let* ((n (length wlist))
          (old-boundaries (map (lambda (i)
                                 (ly:moment-mul swing-div
                                   (ly:make-moment i)))
                            (iota n 1)))
          (deltas (map (lambda (ndelta)
                         (ly:moment-mul swing-div
                           (ly:make-moment (* n ndelta))))
                    (calculate-normalized-deltas wlist))))
     (map cons old-boundaries deltas)))


%%% Putting things together

%% Now, a function call
%%
%%    (apply-swing s '(w1 w2 ... wn) start-time music)
%%
%% does the main work. It iterates over chords/notes/rests in music,
%% looking for boundaries between events that fall on a line of the
%% straight grid. If it finds such a boundary on grid line i, it will
%% attempt to move it by di (calculated as described above) as
%% follows:
%%
%% * Lengthen the event that ends on the boundary by di, unless its
%%   duration is shorter than swing-div.
%%
%% * Shorten the following events (durations: e1, e2, ... ej, ...) by
%%   at most di*ej/swing-div each until the lengthening has been
%%   compensated for completely.

#(define (apply-swing swing-div wlist start-time music)
   (let* ((swing-unit (ly:make-moment (* (length wlist)
                                        (ly:moment-main swing-div))))
          (delta-alist (calculate-delta-alist wlist swing-div))
          (prev-lengthening ZERO-MOMENT)
          (rem-lengthening ZERO-MOMENT))
     (ly:debug "apply-swing: delta-alist: ~a" delta-alist)
     (map-events-with-timing
      (lambda (evt evt-time)
        (let* ((evt-duration (ly:music-length evt))
               (grid-pos-start (ly:moment-mod evt-time swing-unit))
               (grid-pos-end (ly:moment-mod
                              (ly:moment-add evt-time evt-duration)
                              swing-unit)))
          (ly:debug
           "apply-swing: check ~a at ~a (gridpos ~a), length ~a, end gridpos ~a"
           (ly:music-property evt 'name)
           evt-time
           grid-pos-start
           evt-duration
           grid-pos-end)
          ;; Potentially shorten this event to compensate for previous
          ;; lengthenings, but for notes shorter than swing-div,
          ;; restrict subtraction to a fraction evt-duration/swing-div
          ;; of prev-lengthening. (Notice that the formulae must also
          ;; work for negative previous lengthening.)
          (if
           (and (ly:moment<? ZERO-MOMENT (moment-abs rem-lengthening))
                (ly:moment<? ZERO-MOMENT evt-duration))
           (let* ((max-shorten (ly:moment-mul prev-lengthening
                                 (ly:moment-div evt-duration
                                   swing-div)))
                  (shorten-by
                   (if (ly:moment<? (ly:make-moment 1/1)
                         (ly:moment-div max-shorten rem-lengthening))
                       rem-lengthening
                       max-shorten)))
             (set! rem-lengthening
                   (ly:moment-sub rem-lengthening shorten-by))
             (ly:debug
              "apply-swing:       shorten by ~a (remaining: ~a out of ~a)"
              shorten-by rem-lengthening prev-lengthening)
             (music-lengthen evt (ly:moment-sub ZERO-MOMENT shorten-by))))
          ;; Lengthen notes that end on off-beat grid positions by the
          ;; corresponding delta, if they are long enough
          (if
           (and (assoc grid-pos-end delta-alist)
                (not (ly:moment<? evt-duration swing-div)))
           (let ((delta (assoc-ref delta-alist grid-pos-end)))
             (ly:debug "apply-swing:       lengthen by ~a" delta)
             (music-lengthen evt delta)
             (set! prev-lengthening delta)
             (set! rem-lengthening (ly:moment-add delta rem-lengthening))))
          evt))
      music
      start-time)))


%%% Music function definitions

applySwing =
#(define-music-function
  (swingDiv weightList music)
  (ly:duration? pair? ly:music?)
  (_i "Scale event durations in @var{music} to create a swing feeling.

The groove is defined as follows: Given a @var{weightList} of
@code{#'(w1 w2 ... wn)}, transform groups of n notes of duration
@var{swingDiv} ('swing cycles') such that the new relative lengths of
the notes in a cycle correspond to the ratios of weight numbers
@code{w1}, ..., @code{wn}, while the total duration of each cycle
remains the same.

For example, a @var{weightList} of @code{#'(2 1)} creates a triplet
feel pulse: group pairs of two notes, making the first twice as long
as the second.

@var{music} must start on-beat, i.e. the earliest event in @var{music}
also marks the beginning of the first swing cycle.")
  (apply-swing
   (ly:duration-length swingDiv)
   weightList
   ZERO-MOMENT
   music))

%% FIXME: enter offset as a duration rather than a moment? -vv
applySwingWithOffset =
#(define-music-function
  (swingDiv weightList offset music)
  (ly:duration? pair? ly:moment? ly:music?)
  (_i "Like @code{\\applySwing swingDiv weightList music}, but allow
@var{music} to start off-beat.

Use the argument @var{offset} to specify the start time (as a moment)
of @var{music} relative to the start of the first swing cycle.")
  (apply-swing
   (ly:duration-length swingDiv)
   weightList
   offset
   music))

tripletFeel =
#(define-music-function
  (swingDiv music)
  (ly:duration? ly:music?)
  (_i "Apply a triplet-feel swing to @var{music}, swinging notes of
duration @var{swingDiv}. Equivalent to @code{\\applySwing
swingDiv #'(2 1) music}.")
  (apply-swing
   (ly:duration-length swingDiv)
   '(2 1)
   ZERO-MOMENT
   music))
