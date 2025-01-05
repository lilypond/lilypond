;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2024 Daniel Eble <nine.fierce.ballads@gmail.com>
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

;; If ly:context-property-stack-push and ly:context-property-stack-pop become
;; performance hot spots, consider reimplementing them in C++, eliminating the
;; hash lookup of 'propertyStacks, and storing the stacks in a hash table
;; instead of an alist.

(define ly:context-property-none (make-symbol "never used as a property value"))

(define (ly:context-property-stack-push context name val)
  "Add @var{val} to the stack for property @var{name} in context
@var{context}.  This is low-level access that changes only the stack and not the
property itself."
  (let* ((stacks (ly:context-property context 'propertyStacks
                                      #:search-ancestors? #f))
         (stack (assoc-get name stacks '())))
    (set! (ly:context-property context 'propertyStacks)
          (assoc-set! stacks name (cons val stack)))))

(define (ly:context-property-stack-pop context name)
  "Remove and return the top value from the stack for property @var{name} in
context @var{context}.  If the stack is empty, throw
@code{ly:context-property-stack-underflow}.  This is low-level access that
changes only the stack and not the property itself."
  (let* ((stacks (ly:context-property context 'propertyStacks
                                      #:search-ancestors? #f))
         (stack (assoc-get name stacks '())))
    (when (null? stack)
      (throw 'ly:context-property-stack-underflow))
    (let ((val (car stack)))
      (set! (ly:context-property context 'propertyStacks)
            (assoc-set! stacks name (cdr stack)))
      val)))

(define-public (ly:context-property-push context name)
  "Add the current value (or lack thereof) of property @var{name} in context
@var{context} to a context-specific stack.  The state should be restored later
with a paired @code{ly:context-property-pop} for the same context and property."
  (let ((top (ly:context-property context name
                                  #:default ly:context-property-none
                                  #:search-ancestors? #f)))
    (if (eq? top ly:context-property-none)
        (set! top '())
        (set! top (list top)))
    (ly:context-property-stack-push context name top)
    top))

(define-public (ly:context-property-pop context name)
  "Remove the top entry from the stack for property @var{name} in context
@var{context} and set or unset the property.  If the stack is empty, throw
@code{ly:context-property-stack-underflow} and do not change the property."
  (let ((top (ly:context-property-stack-pop context name)))
    (if (null? top)
        (ly:context-unset-property context name)
        (ly:context-set-property! context name (car top)))))
