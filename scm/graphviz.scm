;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2007--2023 Joe Neeman <joeneeman@gmail.com>
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


(define-module (lily graphviz)
  #:use-module (lily)
  #:use-module (srfi srfi-9)
  #:export
  (make-empty-graph add-node add-edge add-cluster
                    graph-write
                    ))

(define-record-type <graph>
  (make-graph nodes edges clusters name)
  graph?
  (nodes nodes set-nodes!)
  (edges edges set-edges!)
  (clusters clusters set-clusters!)
  ;; Unused field?
  (name name))

(define (make-empty-graph name) (make-graph '() '() '() name))

(define (add-cluster graph node-id cluster-name)
  (let* ((cs (clusters graph))
         (cluster (assoc cluster-name cs))
         (already-in-cluster (if cluster
                                 (cdr cluster)
                                 '())))
    (set-clusters! graph (assoc-set! cs
                                     cluster-name
                                     (cons node-id already-in-cluster)))))

(define (add-node graph label . cluster-name)
  (let* ((ns (nodes graph))
         (id (length ns)))
    (set-nodes! graph (assv-set! ns id label))
    (if (and (not (null? cluster-name))
             (string? (car cluster-name)))
        (add-cluster graph id (car cluster-name)))
    id))

(define (add-edge graph node1 node2)
  (set-edges! graph (cons `(,node1 . ,node2) (edges graph))))

(define (graph-write graph out)
  (let ((ns (nodes graph))
        (es (edges graph))
        (cs (clusters graph)))
    (if (file-port? out)
        (ly:message (G_ "Writing graph to `~a'...\n") (port-filename out)))
    (display "digraph G {\nrankdir=\"LR\"\nnode [shape=rectangle]\n" out)
    (for-each (lambda (n) (format out "~a [label=\"~a\"]\n" (car n) (cdr n)))
              ns)
    (for-each (lambda (e) (format out "~a -> ~a\n" (car e) (cdr e)))
              es)
    (for-each (lambda (c)
                (format out "subgraph cluster_~a {\nlabel= \"~a\"\ncolor=blue\n"
                        (string-filter char-alphabetic? (car c))
                        (car c))
                (for-each (lambda (n) (format out "~a\n" n)) (cdr c))
                (display "}\n" out))
              cs)
    (display "}" out)))
