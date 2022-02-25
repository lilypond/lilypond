\version "2.21.0"

\header {

  texidoc = "This is a test of combining post-events with various
constructs.  Problems are reported on the stderr of this run; there
are no images produced."

}

%%%
%%% Testing utilities
%%%

testIt =
#(define-void-function (harmless music) ((boolean?) ly:music?)
   ;; We check whether there is sequential music with a single
   ;; expression that has the location of its single element basically
   ;; at the end of the expression:
   ;;
   ;; That's when the expression has been combined immediately at
   ;; parse time and not in a second sweep
   (define (contained? m p)
     (let ((m-loc (ly:input-both-locations (ly:music-property m 'origin)))
           (p-loc (ly:input-both-locations (ly:music-property p 'origin))))
       (and (string= (first m-loc) (first p-loc))
            (= (fourth m-loc) (fourth p-loc))
            (>= (fifth m-loc) (fifth p-loc)))))
   (let* ((musl (and (music-is-of-type? music 'sequential-music)
                     (ly:music-property music 'elements)))
          (muse (and (pair? musl) (not (pair? (cdr musl))) (car musl)))
          (badpost (and muse
                        (find (lambda (m) (not (contained? muse m)))
                              (extract-typed-music muse 'post-event)))))
     (cond ((not muse)
            (ly:music-error music (G_ "expected single-element sequential expression")))
           (badpost
            (if harmless
                (ly:music-message badpost (G_ "late post-event, expected"))
                (ly:music-warning badpost (G_ "late post-event, unexpected"))))
           (else
            (if harmless
                (ly:music-warning muse (G_ "no late post-event, unexpected")))))))

pitch = c'
note = c'4
chord = <c' g'>
lyr = \lyricmode { two words }

\testIt ##t { \note -1 }
\testIt ##t { $note -1 }
\testIt ##t { \chord -1 }
\testIt { { \note -1 } }
\testIt ##t { { \note -1 } -2 }
\testIt { < \note -1 g' > -2 }
\testIt { { \chord -1 } }
\testIt { $pitch -1 }
\testIt { c-\single \slurUp ( -1 }
\testIt ##t { c \single \slurUp ( -1 }
\testIt ##t \lyricmode { \lyr __ }
\testIt ##t \lyricmode { \lyr -- }

%% end test.
