;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2021--2023 Jean Abou Samra <jean@abou-samra.fr>
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

;; Many node names from the Internals Reference are camel-cased, e.g.,
;; NoteHead.  In "See also" sections, they appear in a normal (not typewriter)
;; font; therefore, TeX happily hyphenates them in ugly places, like
;; No-teHead.  This generates an includable Texinfo file containing hyphenation
;; rules to force breaking at word boundaries, like Note-Head.

;; TODO: make it work for Translator_names?  The use of underscores seems
;; to make this a nightmare.

(define hyphenation-rules-string
  (let ((music-types (map car music-descriptions))
        (context-types (map car (ly:output-description $defaultlayout)))
        (grob-types (map car all-grob-descriptions))
        ;; This is enough because we don't use non-ASCII characters in internal
        ;; naming.
        (uppercase (ly:make-regex "[A-Z]")))
    (define (hyphenate-camel-case name)
      (define (maybe-hyphen match)
        ;; Don't add a leading hyphen at the start of the string.
        (if (zero? (car (ly:regex-match-positions match)))
            ""
            "-"))
      (ly:regex-replace uppercase name maybe-hyphen 0))
    (string-append
     "@hyphenation{\n"
     (apply string-append
            (sort
             (map
              (lambda (sym)
                (format #f "  ~a\n"
                        (hyphenate-camel-case (symbol->string sym))))
              (append music-types context-types grob-types))
             string<?))
     "}\n")))
