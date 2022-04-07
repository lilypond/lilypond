;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2013--2022 Mike Solomon <mike@mikesolomon.org>
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

(define-public (skyline-pair::empty? skyp)
  (and (ly:skyline-empty? (car skyp))
       (ly:skyline-empty? (cdr skyp))))

;; checks if the pair is not null, and then if not empty
(define-public (skyline-pair-and-non-empty? skyp)
  (and (ly:skyline-pair? skyp)
       (not (skyline-pair::empty? skyp))))
