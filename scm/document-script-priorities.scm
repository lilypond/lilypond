;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2023--2023 Werner Lemberg <wl@gnu.org>
;;;;                          Jean Abou Samra <jean@abou-samra.fr>
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

(define script-priorities-doc-string
  (format #f
          "@indentedblock
@multitable {@code{AccidentalSuggestion}} {Priority}
@headitem Grob or Script @tab Priority
~a
@end multitable
@end indentedblock
"
          (string-join
           (priority-entries `(,all-grob-descriptions ,default-script-alist)
                             'script-priority)
           "\n")))
