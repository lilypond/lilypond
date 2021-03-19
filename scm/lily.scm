;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2020 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;
;;;; LilyPond is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LilyPond is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

;; Internationalisation: (_i "to be translated") gets an entry in the
;; POT file; (gettext ...) must be invoked explicitly to do the actual
;; "translation".
;;
;; (define-macro (_i x) x)
;; (define-macro-public _i (x) x)
;; (define-public-macro _i (x) x)
;; Abbrv-PWR!

(defmacro-public _i (x) x)

;; GUILE defaults to fixed seed.
(define-public (randomize-rand-seed)
  (ly:randomize-rand-seed)
  (let*
      ((t (gettimeofday))
       ;; GUILE's random initialization is clumsy (it converts the
       ;; integer to decimal string, and then sums groups of 8 bytes
       ;; as 64 bit integers.  Here we multiply to spread the entropy
       ;; around a bit better.
       (seed (*
              (cdr t)
              (car t)
              (getpid))))
    (set! *random-state* (seed->random-state seed))))

(randomize-rand-seed)

(read-enable 'positions)
(cond-expand
 (guile-2
  (begin
    (debug-enable 'backtrace)
    (debug-set! show-file-name #t)))
 (else (debug-enable 'debug)))

(define-public PLATFORM
  (string->symbol
   (string-downcase
    (car (string-tokenize (utsname:sysname (uname)) char-set:letter)))))

;; We don't use (srfi srfi-39) (parameter objects) here because that
;; does not give us a name/handle to the underlying fluids themselves.

(define %parser (make-fluid))
(define %location (make-fluid))
;; No public setters: should not get overwritten in action
(define-public (*parser*) (fluid-ref %parser))
(define-public (*location*) (fluid-ref %location))
;; but properly scoped location should be fine
(defmacro-public with-location (loc . body)
  `(with-fluids ((,%location ,loc)) ,@body))

(define-public _ gettext)

;; Guile 1.8 doesn't have (and doesn't need) eval-when.
(defmacro-public eval-early (expr)
  `(cond-expand
    (guile-2 (eval-when (expand load eval) ,expr))
    (else ,expr)))

;; It would be nice to convert occurences of parser/location to
;; (*parser*)/(*location*) using the syncase module but it is utterly
;; broken in GUILE 1 and would require changing a lot of unrelated
;; innocuous constructs which just happen to fall apart with
;; inscrutable error messages.

;;
;; Session-handling variables and procedures.
;;
;;  A "session" corresponds to one .ly file processed on a LilyPond
;;  command line.  Every session gets to see a reasonably fresh state
;;  of LilyPond and should work independently from previous files.
;;
;;  Session management relies on cooperation, namely the user not
;;  trying to change variables and data structures internal to
;;  LilyPond.  It is not proof against in-place modification of data
;;  structures (as they are just reinitialized with the original
;;  identities), and it is not proof against tampering with internals.
;;
;;  As a consequence, session management is not sufficient for
;;  separating multiple independent .ly files in "-dsafe" mode: you
;;  should give each its own LilyPond process when reliable separation
;;  is mandatory.
;;
;;  For standard tasks and programming practices, multiple sessions in
;;  the same LilyPond job should work reasonably independently and
;;  without "bleed-over" while still loading and compiling the
;;  relevant .scm and .ly files only once.
;;

(define lilypond-declarations '())
(define lilypond-exports '())
(define after-session-hook (make-hook))

(define-public (call-after-session thunk)
  (if (ly:undead? lilypond-declarations)
      (ly:error (_ "call-after-session used after session start")))
  (add-hook! after-session-hook thunk #t))

(define (make-session-variable name value)
  (if (ly:undead? lilypond-declarations)
      (ly:error (_ "define-session used after session start")))
  (let ((var (module-make-local-var! (current-module) name)))
    (if (variable-bound? var)
        (ly:error (_ "symbol ~S redefined") name))
    (variable-set! var value)
    var))

(define (define-session-internal name value)
  ;; work function for define-session
  (set! lilypond-declarations
        (cons (make-session-variable name value) lilypond-declarations)))


(defmacro define-session (name value)
  "This defines a variable @var{name} with the starting value
@var{value} that is reinitialized at the start of each session.
A@tie{}session basically corresponds to one LilyPond file on the
command line.  The value is recorded at the start of the first session
after loading all initialization files and before loading the user
file and is reinstated for all of the following sessions.  This
happens just by replacing the value, not by copying structures, so you
should not destructively modify them.  For example, lists defined in
this manner should be changed within a session only be adding material
to their front or replacing them altogether, not by modifying parts of
them.  It is an error to call @code{define-session} after the first
session has started."
  `(define-session-internal ',name ,value))

(define (define-session-public-internal name value)
  ;; work function for define-session-public
  (set! lilypond-exports
        (acons name (make-session-variable name value) lilypond-exports)))

(defmacro define-session-public (name value)
  "Like @code{define-session}, but also exports @var{name} into parser modules."
  `(begin
     ;; this is a bit icky: we place the variable right into every
     ;; parser module so that both set! and define will affect the
     ;; original variable in the (lily) module.  However, we _also_
     ;; export it normally from (lily) for the sake of other modules
     ;; not sharing the name space of the parser.
     (define-session-public-internal ',name ,value)
     (export ,name)))

(define (session-terminate)
  (if (ly:undead? lilypond-declarations)
      (begin
        (for-each
         (lambda (p) (variable-set! (cadr p) (cddr p)))
         (ly:get-undead lilypond-declarations))
        (run-hook after-session-hook))))

(define lilypond-interfaces #f)

(define-public (session-initialize thunk)
  "Initialize this session.  The first session in a LilyPond run is
initialized by calling @var{thunk}, then recording the values of all
variables in the current module as well as those defined with
@code{define-session}.  Subsequent calls of @code{session-initialize}
ignore @var{thunk} and instead just reinitialize all recorded
variables to their value after the initial call of @var{thunk}."

  ;; We need to save the variables of the current module along with
  ;; their values: functions defined in the module might refer to the
  ;; variables.

  ;; The entries in lilypond-declarations consist of a cons* consisting
  ;; of symbol, variable, and value.  Variables defined with
  ;; define-session have the symbol set to #f.

  (if (ly:undead? lilypond-declarations)
      (begin
        (module-use-interfaces! (current-module) (reverse lilypond-interfaces))
        (for-each
         (lambda (p)
           (let ((var (cadr p))
                 (val (cddr p)))
             (variable-set! var val)
             (if (car p)
                 (module-add! (current-module) (car p) var))))
         (ly:get-undead lilypond-declarations)))
      (begin
        ;; import all public session variables natively into parser
        ;; module.  That makes them behave identically under define/set!
        (for-each (lambda (v)
                    (module-add! (current-module) (car v) (cdr v)))
                  lilypond-exports)
        ;; Initialize first session
        (thunk)
        ;; lilypond-exports is no longer needed since we will grab its
        ;; values from (current-module).
        (set! lilypond-exports #f)
        (set! lilypond-interfaces
              (filter (lambda (m) (eq? 'interface (module-kind m)))
                      (module-uses (current-module))))
        (let ((decl (map! (lambda (v)
                            (cons* #f v (variable-ref v)))
                          lilypond-declarations)))
          (module-for-each
           (lambda (s v)
             (let ((val (variable-ref v)))
               (if (and (not (eq? s '%module-public-interface)) (not (ly:lily-parser? val)))
                   (set! decl
                         (cons
                          (cons* s v val)
                          decl)))))
           (current-module))
          (set! lilypond-declarations (ly:make-undead decl))))))

(define scheme-options-definitions
  `(
    ;; NAMING: either

    ;; - [subject-]object-object-verb +"ing"
    ;; - [subject-]-verb-object-object

    ;; Avoid overlong lines in `lilypond -dhelp'!  Strings should not
    ;; be longer than 48 characters per line.

    (anti-alias-factor 1
                       "Render at higher resolution (using given
positive integer factor <=8) and scale down
result to prevent jaggies in PNG images.")
    (aux-files #t
               "Create .tex, .texi, .count files in the
EPS backend.")
    (backend ps
             "Select backend.  Possible values: 'eps, 'null,
'ps, 'scm, 'svg.")
    (check-internal-types #f
                          "Check every property assignment for types.")
    (clip-systems #f
                  "Generate cut-out snippets of a score.")
    (crop #f
          "Match the size of the normal output to the
typeset image.")
    (datadir #f
             "LilyPond prefix for data files (read-only).")
    (debug-gc #f
              "Dump memory debugging statistics.")
    (debug-gc-assert-parsed-dead #f
                                 "For memory debugging: Ensure that all
references to parsed objects are dead.  This is
an internal option, and is switched on
automatically for `-ddebug-gc'.")
    (debug-lexer #f
                 "Debug the flex lexer.")
    (debug-page-breaking-scoring #f
                                 "Dump scores for many different page breaking
configurations.")
    (debug-parser #f
                  "Debug the bison parser.")
    (debug-property-callbacks #f
                              "Debug cyclic callback chains.")
    (debug-skylines #f
                    "Debug skylines.")
    (delete-intermediate-files #t
                               "Delete unusable, intermediate PostScript files.")
    (dump-signatures #f
                     "Dump output signatures of each system.")
    (embed-source-code #f
                       "Embed the source files inside the generated PDF
document.")
    (eps-box-padding #f
                     "Pad left edge of the output EPS bounding box by
given amount (in mm).")
    (font-export-dir #f
                     "Directory for exporting fonts as PostScript
files.")
    (font-ps-resdir #f
                    "Build a subset of PostScript resource directory
for embedding fonts.")
    (gs-api #t
            "Whether to use the Ghostscript API (read-only
if not available).")
    (gs-load-fonts #f
                   "Load fonts via Ghostscript.")
    (gs-load-lily-fonts #f
                        "Load only LilyPond fonts via Ghostscript.")
    (gs-never-embed-fonts #f
                          "Make Ghostscript embed only TrueType fonts and
no other font format.")
    (gui #f
         "Run LilyPond from a GUI and redirect stderr to
a log file.")
    (help #f
          "Show this help.")
    (include-book-title-preview #t
                                "Include book titles in preview images.")
    (include-eps-fonts #t
                       "Include fonts in separate-system EPS files.")
    (include-settings #f
                      "If string FOO is given as an argument, include
file `FOO' (using LilyPond syntax) for global
settings, included before the score is
processed.")
    (job-count #f
               "Process in parallel, using the given number of
jobs.")
    (log-file #f
              "If string FOO is given as an argument, redirect
output to log file `FOO.log'.")
    (max-markup-depth 1024
                      "Maximum depth for the markup tree.  If a markup
has more levels, assume it will not terminate
on its own, print a warning and return a null
markup instead.")
    (midi-extension ,(if (eq? PLATFORM 'windows)
                         "mid"
                         "midi")
                    "Set the default file extension for MIDI output
file to given string.")
    (music-font-encodings #f
                          "Use font encodings and the PostScript `show'
operator with music fonts.")
    (music-strings-to-paths #f
                            "Convert text strings to paths when glyphs
belong to a music font.")
    (outline-bookmarks #t
                       "Use bookmarks in table of contents metadata
(e.g., for PDF viewers).")
    (paper-size "a4"
                "Set default paper size.")
    (pixmap-format "png16m"
                   "Set GhostScript's output format for pixel
images.")
    (png-width 0
               "Image width for PNG output (in pixels).")
    (png-height 0
                "Image height for PNG output (in pixels).")
    (point-and-click #t
                     "Add point & click links to PDF and SVG output.")
    (preview #f
             "Create preview images also.")
    (print-pages #t
                 "Print pages in the normal way.")
    (profile-property-accesses #f
                               "Keep statistics of get_property() calls.")
    (protected-scheme-parsing #t
                              "Continue when errors in inline Scheme are
caught in the parser.  If #f, halt on errors
and print a stack trace.")
    (read-file-list #f
                    "Specify name of a file which contains a list of
input files to be processed.")
    (relative-includes #t
                       "When processing an \\include command, look for
the included file relative to the current file\
\n(instead of the root file).")
    (resolution 101
                "Set resolution for generating PNG pixmaps to
given value (in dpi).")
    (safe #f
          "Run in safer mode.")
    (separate-log-files #f
                        "For input files `FILE1.ly', `FILE2.ly', ...
output log data to files `FILE1.log',
`FILE2.log', ...")
    (show-available-fonts #f
                          "List available font names.")
    (strict-infinity-checking #f
                              "Force a crash on encountering Inf and NaN
floating point exceptions.")
    (strip-output-dir #t
                      "Don't use directories from input files while
constructing output file names.")
    (strokeadjust #f
                  "Set the PostScript `strokeadjust' operator
explicitly.  This employs different drawing
primitives, resulting in large PDF file size
increases but often markedly better PDF
previews.")
    (svg-woff #f
              "Use woff font files in SVG backend.")
    (verbose ,(ly:verbose-output?)
             "Verbose output, i.e., loglevel at least DEBUG
(read-only).")
    (warning-as-error #f
                      "Change all warning and programming_error
messages into errors.")
    ))

;; Need to do this in the beginning.  Other parts of the Scheme
;; initialization depend on these options.

(for-each (lambda (x)
            (ly:add-option (car x) (cadr x) (caddr x)))
          scheme-options-definitions)

(for-each (lambda (x)
            (ly:set-option (car x) (cdr x)))
          (with-input-from-string (ly:command-line-options) read))

(debug-set! stack 0)

(if (defined? 'set-debug-cell-accesses!)
    (set-debug-cell-accesses! #f))

;;(set-debug-cell-accesses! 1000)

(use-modules (ice-9 regex)
             (ice-9 safe)
             (ice-9 format)
             (ice-9 rdelim)
             (ice-9 optargs)
             (oop goops)
             (srfi srfi-1)
             (srfi srfi-13)
             (srfi srfi-14)
             (scm clip-region)
             (scm safe-utility-defs))

;;; There are new modules defined in Guile V2.0 which we need to use.
;;
;;  Modules and scheme files loaded by lily.scm use currying
;;  in Guile V2 this needs a module which is not present in Guile V1.8
;;

(cond-expand
 (guile-2
  (ly:debug (_ "Using (ice-9 curried-definitions) module\n"))
  (use-modules (ice-9 curried-definitions)))
 (else
  (ly:debug (_ "Guile 1.8\n"))))

;; TODO add in modules for V1.8.7 deprecated in V2.0 and integrated
;; into Guile base code, like (ice-9 syncase).
;;

(define-public fancy-format
  format)

(define-public (ergonomic-simple-format dest . rest)
  "Like ice-9's @code{format}, but without the memory consumption."
  (if (string? dest)
      (apply simple-format #f dest rest)
      (apply simple-format dest rest)))

(define format
  ergonomic-simple-format)

;; my display
(define-public (myd k v)
  (display k)
  (display ": ")
  (display v)
  (display ", ")
  v)

(define-public (print . args)
  (apply format (current-output-port) args))


;;; General settings.
;;;
;;; Debugging evaluator is slower.  This should have a more sensible
;;; default.


(if (or (ly:get-option 'verbose))
    (begin
      (ly:set-option 'protected-scheme-parsing #f)
      (debug-enable 'backtrace)
      (read-enable 'positions)))

(define music-string-to-path-backends
  '(svg))

(if (memq (ly:get-option 'backend) music-string-to-path-backends)
    (ly:set-option 'music-strings-to-paths #t))

(define-public (ly:load x)
  (let* ((file-name (%search-load-path x)))
    (ly:debug "[~A" file-name)
    (if (not file-name)
        (ly:error (_ "cannot find: ~A") x))
    (primitive-load-path file-name)  ;; to support Guile V2 autocompile
    ;; TODO: Any chance to use ly:debug here? Need to extend it to prevent
    ;;       a newline in this case
    (if (ly:get-option 'verbose)
        (ly:progress "]\n"))))

(define-public DOS
  (let ((platform (string-tokenize
                   (vector-ref (uname) 0) char-set:letter+digit)))
    (if (null? (cdr platform)) #f
        (member (string-downcase (cadr platform)) '("95" "98" "me")))))

(define (slashify x)
  (if (string-index x #\\)
      x
      (string-regexp-substitute
       "//*" "/"
       (string-regexp-substitute "\\\\" "/" x))))

(define-public (ly-getcwd)
  (if (eq? PLATFORM 'windows)
      (slashify (getcwd))
      (getcwd)))

(define-public (is-absolute? file-name)
  (let ((file-name-length (string-length file-name)))
    (if (= file-name-length 0)
        #f
        (or (eq? (string-ref file-name 0) #\/)
            (and (eq? PLATFORM 'windows)
                 (> file-name-length 2)
                 (eq? (string-ref file-name 1) #\:)
                 (or (eq? (string-ref file-name 2) #\\)
                     (eq? (string-ref file-name 2) #\/)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; If necessary, emulate Guile V2 module_export_all! for Guile V1.8.n
(cond-expand
 (guile-2 #f)
 (else
  (define (module-export-all! mod)
    (define (fresh-interface!)
      (let ((iface (make-module)))
        (set-module-name! iface (module-name mod))
        ;; for guile 2: (set-module-version! iface (module-version mod))
        (set-module-kind! iface 'interface)
        (set-module-public-interface! mod iface)
        iface))
    (let ((iface (or (module-public-interface mod)
                     (fresh-interface!))))
      (set-module-obarray! iface (module-obarray mod))))))


(define-safe-public (lilypond-version)
  (string-join
   (map (lambda (x) (if (symbol? x)
                        (symbol->string x)
                        (number->string x)))
        (ly:version))
   "."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init pitch system

(ly:set-default-scale (ly:make-scale #(0 1 2 5/2 7/2 9/2 11/2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other files.

;;
;;  List of Scheme files to be loaded into the (lily) module.
;;
;;  - Library definitions, need to be at the head of the list
(define init-scheme-files-lib
  '("lily-library.scm"
    "output-lib.scm"))
;;  - Files containing definitions used later by other files later in load
(define init-scheme-files-used
  '("markup-macros.scm"
    "parser-ly-from-scheme.scm"))
;;  - Main body of files to be loaded
(define init-scheme-files-body
  '("file-cache.scm"
    "define-event-classes.scm"
    "define-music-callbacks.scm"
    "define-music-types.scm"
    "define-note-names.scm"
    "c++.scm"
    "chord-entry.scm"
    "skyline.scm"
    "markup.scm"
    "define-markup-commands.scm"
    "stencil.scm"
    "modal-transforms.scm"
    "chord-ignatzek-names.scm"
    "music-functions.scm"
    "part-combiner.scm"
    "autochange.scm"
    "define-music-properties.scm"
    "time-signature.scm"
    "time-signature-settings.scm"
    "auto-beam.scm"
    "chord-name.scm"
    "bezier-tools.scm"

    "define-context-properties.scm"
    "translation-functions.scm"
    "script.scm"
    "midi.scm"
    "layout-beam.scm"
    "parser-clef.scm"
    "layout-slur.scm"
    "font.scm"
    "encoding.scm"

    "bar-line.scm"
    "flag-styles.scm"
    "fret-diagrams.scm"
    "tablature.scm"
    "harp-pedals.scm"
    "define-woodwind-diagrams.scm"
    "display-woodwind-diagrams.scm"
    "predefined-fretboards.scm"
    "define-grob-properties.scm"
    "define-grobs.scm"
    "define-grob-interfaces.scm"
    "define-stencil-commands.scm"
    "scheme-engravers.scm"
    "titling.scm"

    "paper.scm"
    "backend-library.scm"
    "color.scm"))
;;  - Files to be loaded last
(define init-scheme-files-tail
  ;;  - must be after everything has been defined
  '("safe-lily.scm"))
;;
;; Now construct the load list
;;
(define init-scheme-files
  (append init-scheme-files-lib
          init-scheme-files-used
          init-scheme-files-body
          init-scheme-files-tail))

(for-each ly:load init-scheme-files)

(define-public r5rs-primary-predicates
  `((,boolean? . "boolean")
    (,char? . "character")

    (,list? . "list")
    (,null? . "null")

    (,number? . "number")
    (,complex? . "complex number")
    (,integer? . "integer")
    (,rational? . "rational number")
    (,real? . "real number")

    (,pair? . "pair")

    (,port? . "port")
    (,input-port? . "input port")
    (,output-port? . "output port")
    (,eof-object? . "end-of-file object")

    (,procedure? . "procedure")
    (,string? . "string")
    (,symbol? . "symbol")
    (,vector? . "vector")))

(define-public r5rs-secondary-predicates
  `((,char-alphabetic? . "alphabetic character")
    (,char-lower-case? . "lower-case character")
    (,char-numeric? . "numeric character")
    (,char-upper-case? . "upper-case character")
    (,char-whitespace? . "whitespace character")

    (,even? . "even number")
    (,exact? . "exact number")
    (,inexact? . "inexact number")
    (,negative? . "negative number")
    (,odd? . "odd number")
    (,positive? . "positive number")
    (,zero? . "zero")
    ))

(define-public guile-predicates
  `((,hash-table? . "hash table")
    ))

(define-public lilypond-scheme-predicates
  `((,boolean-or-symbol? . "boolean or symbol")
    (,color? . "color")
    (,cheap-list? . "list")
    (,fraction? . "fraction, as pair")
    (,grob-list? . "list of grobs")
    (,index? . "non-negative integer")
    (,integer-or-markup? . "integer or markup")
    (,key? . "index or symbol")
    (,key-list? . "list of indexes or symbols")
    (,key-list-or-music? . "key list or music")
    (,key-list-or-symbol? . "key list or symbol")
    (,markup? . "markup")
    (,markup-command-list? . "markup command list")
    (,markup-list? . "markup list")
    (,moment-pair? . "pair of moment objects")
    (,number-list? . "number list")
    (,number-or-grob? . "number or grob")
    (,number-or-pair? . "number or pair")
    (,number-or-string? . "number or string")
    (,number-pair? . "pair of numbers")
    (,number-pair-list? . "list of number pairs")
    (,rational-or-procedure? . "an exact rational or procedure")
    (,rhythmic-location? . "rhythmic location")
    (,scale? . "non-negative rational, fraction, or moment")
    (,scheme? . "any type")
    (,string-or-pair? . "string or pair")
    (,string-or-music? . "string or music")
    (,string-or-symbol? . "string or symbol")
    (,symbol-list? . "symbol list")
    (,symbol-list-or-music? . "symbol list or music")
    (,symbol-list-or-symbol? . "symbol list or symbol")
    (,void? . "void")
    ))

(define-public lilypond-exported-predicates
  `((,ly:book? . "book")
    (,ly:box? . "box")
    (,ly:context? . "context")
    (,ly:context-def? . "context definition")
    (,ly:context-mod? . "context modification")
    (,ly:dimension? . "dimension, in staff space")
    (,ly:dir? . "direction")
    (,ly:dispatcher? . "dispatcher")
    (,ly:duration? . "duration")
    (,ly:event? . "post event")
    (,ly:font-metric? . "font metric")
    (,ly:grob? . "graphical (layout) object")
    (,ly:grob-array? . "array of grobs")
    (,ly:grob-properties? . "grob properties")
    (,ly:input-location? . "input location")
    (,ly:item? . "item")
    (,ly:iterator? . "iterator")
    (,ly:lily-lexer? . "lily-lexer")
    (,ly:lily-parser? . "lily-parser")
    (,ly:listener? . "listener")
    (,ly:moment? . "moment")
    (,ly:music? . "music")
    (,ly:music-function? . "music function")
    (,ly:music-list? . "list of music objects")
    (,ly:music-output? . "music output")
    (,ly:otf-font? . "OpenType font")
    (,ly:output-def? . "output definition")
    (,ly:page-marker? . "page marker")
    (,ly:pango-font? . "pango font")
    (,ly:paper-book? . "paper book")
    (,ly:paper-system? . "paper-system Prob")
    (,ly:pitch? . "pitch")
    (,ly:prob? . "property object")
    (,ly:score? . "score")
    (,ly:skyline? . "skyline")
    (,ly:skyline-pair? . "pair of skylines")
    (,ly:source-file? . "source file")
    (,ly:spanner? . "spanner")
    (,ly:spring? . "spring")
    (,ly:stencil? . "stencil")
    (,ly:stream-event? . "stream event")
    (,ly:transform? . "coordinate transform")
    (,ly:translator? . "translator")
    (,ly:translator-group? . "translator group")
    (,ly:undead? . "undead container")
    (,ly:unpure-pure-container? . "unpure/pure container")
    ))


(set! type-p-name-alist
      (append r5rs-primary-predicates
              r5rs-secondary-predicates
              guile-predicates
              lilypond-scheme-predicates
              lilypond-exported-predicates))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; debug memory leaks

(define gc-dumping
  #f)

(define gc-protect-stat-count
  0)

;; Undead objects that should be ignored after the first time round
(define gc-zombies
  (make-weak-key-hash-table 0))

(define-public (dump-live-object-stats outfile)
  (for-each (lambda (x)
              (format outfile "~a: ~a\n" (car x) (cdr x)))
            (sort (gc-live-object-stats)
                  (lambda (x y)
                    (string<? (car x) (car y))))))

(define-public (dump-gc-protects)
  (set! gc-protect-stat-count (1+ gc-protect-stat-count))
  (let* ((protects (sort (hash-table->alist (ly:protects))
                         (lambda (a b)
                           (< (object-address (car a))
                              (object-address (car b))))))
         (out-file-name (string-append
                         "gcstat-" (number->string gc-protect-stat-count)
                         ".scm"))
         (outfile (open-file out-file-name "w")))
    (set! gc-dumping #t)
    (ly:progress "Dumping GC statistics ~a...\n" out-file-name)
    (for-each (lambda (y)
                (let ((x (car y))
                      (c (cdr y)))
                  (format outfile "~a (~a) = ~a\n" (object-address x) c x)))
              (filter
               (lambda (x)
                 (not (symbol? (car x))))
               protects))
    (format outfile "\nprotected symbols: ~a\n"
            (apply + (map (lambda (obj-count)
                            (if (symbol? (car obj-count))
                                (cdr obj-count)
                                0))
                          protects)))

    ;; (display (ly:smob-protects))
    (newline outfile)
    (if (defined? 'gc-live-object-stats)
        (let* ((stats #f))
          (ly:progress "Live object statistics: GC'ing\n")
          (ly:reset-all-fonts)
          (gc)
          (gc)
          (ly:progress "Asserting dead objects\n")
          (ly:set-option 'debug-gc-assert-parsed-dead #t)
          (gc)
          (ly:set-option 'debug-gc-assert-parsed-dead #f)
          (for-each
           (lambda (x)
             (if (not (hashq-ref gc-zombies x))
                 (begin
                   (ly:programming-error "Parsed object should be dead: ~a" x)
                   (hashq-set! gc-zombies x #t))))
           (ly:parsed-undead-list!))
          (set! stats (gc-live-object-stats))
          (ly:progress "Dumping live object statistics.\n")
          (dump-live-object-stats outfile)))
    (newline outfile)
    (let* ((stats (gc-stats)))
      (for-each (lambda (sym)
                  (format outfile "~a ~a ~a\n"
                          gc-protect-stat-count
                          sym
                          (assoc-get sym stats "?")))
                '(protected-objects bytes-malloced cell-heap-size)))
    (set! gc-dumping #f)
    (close-port outfile)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (multi-fork count)
  "Split this process into COUNT helpers.  Returns either a list of
PIDs or the number of the process."
  (define (helper count acc)
    (if (> count 0)
        (let* ((pid (primitive-fork)))
          (if (= pid 0)
              (begin
                (randomize-rand-seed)
                (1- count))
              (helper (1- count) (cons pid acc))))
        acc))

  (helper count '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define* (ly:exit status #:optional (silently #f))
  "Exit function for lilypond"
  (if (ly:get-option 'gs-api)
      (ly:shutdown-gs))
  (if (not silently)
      (case status
        ((0) (ly:basic-progress (_ "Success: compilation successfully completed")))
        ((1) (ly:warning (_ "Compilation completed with warnings or errors")))
        (else (ly:message ""))))
  (exit status))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (lilypond-main files)
  "Entry point for LilyPond."
  (eval-string (ly:command-line-code))
  (if (ly:get-option 'help)
      (begin (ly:option-usage)
             (ly:exit 0 #t)))
  (if (ly:get-option 'show-available-fonts)
      (begin (ly:font-config-display-fonts)
             (ly:exit 0 #t)))
  (if (ly:get-option 'gui)
      (gui-main files))
  (if (null? files)
      (begin (ly:usage)
             (ly:exit 2 #t)))
  (if (ly:get-option 'read-file-list)
      (set! files
            (remove string-null?
                    (append-map
                     (lambda (f)
                       (string-split
                        (cond-expand
                         (guile-2 (string-delete #\cr (ly:gulp-file f)))
                         (else (string-delete (ly:gulp-file f) #\cr)))
                        #\nl))
                     files))))
  (if (and (number? (ly:get-option 'job-count))
           (>= (length files) (ly:get-option 'job-count)))
      (let* ((count (ly:get-option 'job-count))
             (split-todo (split-list files count))
             (joblist (multi-fork count))
             (errors '()))
        (if (not (string-or-symbol? (ly:get-option 'log-file)))
            (ly:set-option 'log-file "lilypond-multi-run"))
        (if (number? joblist)
            (begin (ly:set-option
                    'log-file (format #f "~a-~a"
                                      (ly:get-option 'log-file) joblist))
                   (set! files (vector-ref split-todo joblist)))
            (begin (ly:progress "\nForking into jobs:  ~a\n" joblist)
                   (for-each
                    (lambda (pid)
                      (let* ((stat (cdr (waitpid pid))))
                        (if (not (= stat 0))
                            (set! errors
                                  (acons (list-element-index joblist pid)
                                         stat errors)))))
                    joblist)
                   (for-each
                    (lambda (x)
                      (let* ((job (car x))
                             (state (cdr x))
                             (logfile (format #f "~a-~a.log"
                                              (ly:get-option 'log-file) job))
                             (log (ly:gulp-file logfile))
                             (len (string-length log))
                             (tail (substring  log (max 0 (- len 1024)))))
                        (if (status:term-sig state)
                            (ly:message
                             "\n\n~a\n"
                             (format #f (_ "job ~a terminated with signal: ~a")
                                     job (status:term-sig state)))
                            (ly:message
                             (_ "logfile ~a (exit ~a):\n~a")
                             logfile (status:exit-val state) tail))))
                    errors)
                   (if (pair? errors)
                       (ly:error (_ "Children ~a exited with errors.")
                                 (map car errors)))
                   ;; must overwrite individual entries
                   (if (null? errors)
                       (ly:exit 0 #f)
                       (ly:exit 1 #f))))))

  (if (string-or-symbol? (ly:get-option 'log-file))
      (ly:stderr-redirect (format #f "~a.log" (ly:get-option 'log-file)) "w"))
  (let ((failed (lilypond-all files)))
    (if (pair? failed)
        (begin (ly:error (_ "failed files: ~S") (string-join failed))
               (ly:exit 1 #f))
        (begin
          (ly:exit 0 #f)))))


(define-public (lilypond-all files)

  ;; Do this relatively late (after forking for multiple jobs), so Pango
  ;; can spawn threads (since version 1.48.3) without leading to hangs.
  (ly:reset-all-fonts)

  (let* ((failed '())
         (separate-logs (ly:get-option 'separate-log-files))
         (ping-log
          (and separate-logs
               (if (string-or-symbol? (ly:get-option 'log-file))
                   (open-file (format #f "~a.log" (ly:get-option 'log-file))
                              "a")
                   (fdes->outport 2))))
         (handler (lambda (key failed-file)
                    (set! failed (append (list failed-file) failed)))))
    (cond-expand
     (guile-2 #f)
     (else (gc)))
    (for-each
     (lambda (x)
       (let* ((base (dir-basename x ".ly"))
              (all-settings (ly:all-options)))
         (if separate-logs
             (ly:stderr-redirect (format #f "~a.log" base) "w"))
         (if ping-log
             (format ping-log "Processing ~a\n" base))
         (lilypond-file handler x)
         (ly:check-expected-warnings)
         (session-terminate)
         (for-each (lambda (s)
                     (ly:set-option (car s) (cdr s)))
                   all-settings)

         ;; check that we're not holding on to objects. Doesn't work
         ;; in GUILE 2.x
         (cond-expand
          (guile-2 #f)
          (else
           (begin
             (ly:set-option 'debug-gc-assert-parsed-dead #t)
             (gc)
             (ly:set-option 'debug-gc-assert-parsed-dead #f))
           (for-each
            (lambda (x)
              (if (not (hashq-ref gc-zombies x))
                  (begin
                    (ly:programming-error "Parsed object should be dead: ~a" x)
                    (hashq-set! gc-zombies x #t))))
            (ly:parsed-undead-list!))))

         (if (ly:get-option 'debug-gc)
             (dump-gc-protects)
             (ly:reset-all-fonts))
         (flush-all-ports)))
     files)

    ;; Ensure a notice re failed files is written to aggregate logfile.
    (if ping-log
        (format ping-log "Failed files: ~a\n" failed))
    failed))

(define (lilypond-file handler file-name)
  (catch 'ly-file-failed
         (lambda () (ly:parse-file file-name))
         (lambda (x . args) (handler x file-name))))

(use-modules (scm editor))

(define-public (gui-main files)
  (if (null? files)
      (gui-no-files-handler))
  (if (not (string? (ly:get-option 'log-file)))
      (let* ((base (dir-basename (car files) ".ly"))
             (log-name (string-append base ".log")))
        (if (not (ly:get-option 'gui))
            (ly:message (_ "Redirecting output to ~a...") log-name))
        (ly:stderr-redirect log-name "w")
        (ly:message "# -*-compilation-*-"))
      (let ((failed (lilypond-all files)))
        (if (pair? failed)
            (begin
              ;; ugh
              (ly:stderr-redirect "foo" "r")
              (system (get-editor-command log-name 0 0 0))
              (ly:error (_ "failed files: ~S") (string-join failed))
              ;; not reached?
              (exit 1))
            (ly:exit 0 #f)))))

(define (gui-no-files-handler)
  (let* ((ly (string-append (ly:effective-prefix) "/ly/"))
         ;; FIXME: soft-code, localize
         (welcome-ly (string-append ly "Welcome_to_LilyPond.ly"))
         (cmd (get-editor-command welcome-ly 0 0 0)))
    (ly:message (_ "Invoking `~a'...\n") cmd)
    (system cmd)
    (ly:exit 1 #f)))
