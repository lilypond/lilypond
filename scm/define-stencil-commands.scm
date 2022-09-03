;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

;; TODO:
;;  - stencil-commands should have docstrings.

(define-public (ly:all-stencil-commands)
  "Return the list of stencil commands that can be
defined in the output modules (in files @file{output-*.scm})."
  '(circle
    dashed-line
    draw-line
    ellipse
    embedded-ps
    embedded-svg
    end-group-node
    eps-file
    glyph-string
    grob-cause
    named-glyph
    no-origin
    page-link
    path
    partial-ellipse
    polygon
    resetcolor
    resetrotation
    resetscale
    round-filled-box
    setcolor
    setrotation
    setscale
    start-group-node
    url-link
    utf-8-string
    ))

;; TODO:
;;  - output-backend-commands should have docstrings.

(define-public (ly:all-output-backend-commands)
  "Return the list of extra output backend commands that
are used internally in file @file{lily/@/stencil-interpret.cc}."
  '(color
    combine-stencil
    delay-stencil-evaluation
    footnote
    output-attributes
    rotate-stencil
    scale-stencil
    translate-stencil
    with-outline
    ))

(for-each ly:register-stencil-expression
          (append (ly:all-stencil-commands)
                  (ly:all-output-backend-commands)))
