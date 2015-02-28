;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2011--2015 Bertrand Bordage <bordage.bertrand@gmail.com>
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


(define-public default-string-replacement-alist
  '(;; Whitespaces
    ("\t" . " ")
    ("\n" . " ")
    ("\v" . " ")))

(define-public (internal-add-text-replacements props alist)
  (let* ((dummy-replacements (chain-assoc-get 'replacement-alist props '()))
         (new-replacements
          (append dummy-replacements alist)))
    (prepend-alist-chain 'replacement-alist new-replacements props)))
