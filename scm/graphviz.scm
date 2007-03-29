;;;; graphviz.scm -- utilities for creating graphviz output
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2007 Joe Neeman <joeneeman@gmail.com>


(define-module (scm graphviz)
  #:use-module (lily)
  #:export
  (make-graph add-node add-edge add-cluster
	      graph-write
	      ))

(define (make-graph filename)
   #(() () () ()))


;; fixme: use structs/records.
;; fixme add & use setters.
(define (nodes g) (vector-ref g 1))
(define (edges g) (vector-ref g 2))
(define (clusters g) (vector-ref g 3))

(define (add-cluster graph node-id cluster-name)
  (let* ((cs (clusters graph))
	 (cluster (assq cluster-name cs))
	 (already-in-cluster (if cluster
				 (cdr cluster)
				 '())))
    (vector-set! graph 3 (assq-set! cs
				    cluster-name
				    (cons node-id already-in-cluster)))))

(define (add-node graph label . cluster-name)
  (let* ((ns (nodes graph))
         (id (length ns)))
    (vector-set! graph 1 (cons `(,id . ,label) ns))
    (if (and (not (null? cluster-name))
	     (string? (car cluster-name)))
	(add-cluster graph id (car cluster-name)))
    id))

(define (add-edge graph node1 node2)
  (vector-set! graph 2 (cons `(,node1 . ,node2) (edges graph))))

(define (graph-write graph out)
  (let ((ns (nodes graph))
	(es (edges graph))
	(cs (clusters graph)))
    (ly:message (format (_ "Writing graph `~a'...") (port-filename out)))
    (display "digraph G {\nrankdir=\"LR\"\nnode [shape=rectangle]\n" out)
    (map (lambda (n) (display (format "~a [label=\"~a\"]\n" (car n) (cdr n)) out))
	 ns)
    (map (lambda (e) (display (format "~a -> ~a\n" (car e) (cdr e)) out))
	 es)
    (map (lambda (c)
	  (display (format "subgraph cluster_~a {\nlabel= \"~a\"\ncolor=blue\n"
			   (string-filter (car c) char-alphabetic?)
			   (car c))
		   out)
	  (map (lambda (n) (display (format "~a\n" n) out)) (cdr c))
	  (display "}\n" out))
	 cs)
    (display "}" out)))
