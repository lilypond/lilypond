;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2010--2022 Ian Hulin <ian@hulin.org.uk>
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

;;; Commentary:

;;; This file provides the support routines for a guile debugger called
;;; from an environment controlled by LilyPond.  It works in conjunction
;;; with file guile-debugger.ly.

;;; Code:

(define-module (lily guile-debugger)
  #:use-module (ice-9 debugger)
  #:use-module (ice-9 debugging traps)
  #:use-module (ice-9 debugging trace)
  #:use-module (ice-9 debugging steps)
  #:use-module (ice-9 debugging ice-9-debugger-extensions)
  #:use-module (ice-9 readline)
  #:export (set-break!
            clear-break!
            set-trace-call!
            clear-trace-call!
            set-trace-subtree!
            clear-trace-subtree!
            debug-help))

(define (set-break! proc)
  (install-trap (make <procedure-trap>
                  #:procedure proc
                  #:behaviour debug-trap)))
(define (clear-break! proc)
  (uninstall-trap (make <procedure-trap>
                    #:procedure proc
                    #:behaviour debug-trap)))


(define (set-trace-call! proc)
  (install-trap (make <procedure-trap>
                  #:procedure proc
                  #:behaviour (list trace-trap
                                    trace-at-exit))))
(define (clear-trace-call! proc)
  (uninstall-trap (make <procedure-trap>
                    #:procedure proc
                    #:behaviour (list trace-trap
                                      trace-at-exit))))

(define (set-trace-subtree! proc)
  (install-trap (make <procedure-trap>
                  #:procedure proc
                  #:behaviour (list trace-trap
                                    trace-until-exit))))

(define (clear-trace-subtree! proc)
  (uninstall-trap (make <procedure-trap>
                    #:procedure proc
                    #:behaviour (list trace-trap
                                      trace-until-exit))))

(define (debug-help )
  (display "\nYou may add the following commands as debugging statements in your source file\n")
  (display "or enter the set-x! commands at the guile prompt:\n\n")
  (display " (set-break! <procedure>)\n")
  (display "   causes guile to enter debugger on a call to <procedure>\n")
  (display " (clear-break! <procedure>)\n")
  (display "   disables a breakpoint previously set on a call to <procedure>\n")
  (display " (set-trace-call! <procedure>)\n")
  (display "   prints out a line when Scheme enters or exits <procedure>\n")
  (display " (clear-trace-call! <procedure>)\n")
  (display "   turns off tracing calls to <procedure>\n")
  (display " (set-trace-subtree! <procedure>)\n")
  (display "   displays each line of Scheme code executed during a call to <procedure>\n")
  (display " (clear-trace-subtree! <procedure>)\n")
  (display "   turns off tracing code during calls to <procedure>\n\n")
  (display "Enter help at the guile debug> prompt for further information on debugger commands\n")
  (newline))
