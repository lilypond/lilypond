\version "2.22.1"

\header {
  texidoc = "Pitches for drums may have a defined articulation sign.
This test checks the predefined drum-styles and prints only drum-pitches with an
articulation sign.  These articulations need to be entered as a string,
otherwise they will not be respected and a remark about the missing script
is printed."
}

\paper { indent = 35 }

%% define a test-case to get always one example with a not appropriate defined
%% script-entry
#(module-define! (current-module)
                'test-style
                (alist->hash-table '((bassdrum cross open 0))))

%% the currently predefined styles, see /ly/drumpitch-init.ly
#(define predefined-drumstyles
 '(drums-style
   agostini-drums-style
   timbales-style
   congas-style
   bongos-style
   percussion-style))

$@(map
  (lambda (predefined-style)
    (let* (;; `predefined-drumstyles` is a symbol-list, thus lookup the style in
           ;; (current-module)
           (current-style-hash-table
             (module-ref (current-module) predefined-style))
           ;; transform the hash-tyble into an alist and sort it, to ensure
           ;; reproducibility
           (current-style
             (sort
               (hash-table->alist current-style-hash-table)
               (lambda (p q)
                 (symbol<? (car p) (car q)))))
           ;; keep only drum-pitches with scripts, add remarks about name and
           ;; if the script will be missing in printed output.
           (relevant-drum-notes
             (filter-map
               (lambda (entry)
                 (if (third entry)
                     (make-music
                       'NoteEvent
                       'articulations
                       (list (make-music
                               'TextScriptEvent
                               'direction -1
                               'text (object->string (car entry)))
                             (make-music
                               'TextScriptEvent
                               'direction 1
                               'text
                               (if (string? (third entry))
                                   ""
                                   (make-override-markup '(baseline-skip . 2)
                                     (make-column-markup
                                       (list
                                        "script"
                                        (format #f "~a" (third entry))
                                        "is missing"))))))
                       'drum-type (car entry)
                       'duration (ly:make-duration 2))
                     #f))
               current-style)))

    #{
    \new DrumStaff
      \with {
        instrumentName = #(symbol->string predefined-style)
        drumStyleTable = #current-style-hash-table
        \textLengthOn
      }
      \drummode {
        \cadenzaOn
          %% for styles where no scripts are defined, print a spacer and a remark
          #@(if (null? relevant-drum-notes)
                (list #{ s1^"No scripts defined" #})
                relevant-drum-notes)
        \bar "||"
      }
    #}))
   ;; add the test-style at the end
   (append predefined-drumstyles (list 'test-style)))
