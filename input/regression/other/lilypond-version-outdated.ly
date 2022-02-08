\version "2.23.7"

\header {
  texidoc = "Test for versions considered outdated or not
(used when suggesting @code{convert-ly})."
}

%% This is a unit test since we cannot change the
%% version number of the LilyPond binary that is
%% running.

#(define (expect-outdated expected-result file-version lily-version)
   (let ((result (lilypond-version-outdated? file-version lily-version)))
     (if (not (eq? result expected-result))
         (ly:error "test failed: lilypond-version-outdated? returned ~a on ~a and ~a"
                   result
                   file-version
                   lily-version))))

#(expect-outdated #t '(1 50 46) '(2 22 1))
#(expect-outdated #f '(3 20 46) '(2 22 1))
#(expect-outdated #t '(2 20 50) '(2 22 1))
#(expect-outdated #f '(2 24 0) '(2 22 1))
#(expect-outdated #f '(2 22 0) '(2 22 1))
#(expect-outdated #f '(2 22 1) '(2 22 0))
#(expect-outdated #t '(2 23 0) '(2 23 1))
#(expect-outdated #f '(2 23 1) '(2 23 0))
#(expect-outdated #t '(2 23 0) '(2 23 1 my-patch-level))
