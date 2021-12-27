\version "2.23.6"

\header {
  texidoc = "Pitches for drums may have a defined articulation sign.
This test checks the predefined drum-styles and prints only drum-pitches with an
articulation sign."
}

\paper { indent = 35 }

%% the currently predefined styles, see /ly/drumpitch-init.ly
#(define predefined-drumstyles
 '(drums-style
   agostini-drums-style
   weinberg-drums-style
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
           ;; transform the hash-table into an alist and sort it, to ensure
           ;; reproducibility
           (current-style
             (sort
               (hash-table->alist current-style-hash-table)
               (lambda (p q)
                 (symbol<? (car p) (car q)))))
           ;; keep only drum-pitches with scripts, and
           ;; add label with the appropriate name.
           (relevant-drum-notes
             (filter-map
               (lambda (entry)
                 (if (third entry)
                     (make-music
                       'NoteEvent
                       'articulations
                       (list (make-music
                               'TextScriptEvent
                               'direction DOWN
                               'text (object->string (car entry))))
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
   predefined-drumstyles)
