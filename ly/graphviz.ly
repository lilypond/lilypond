\version "2.11.15"

#(use-modules (scm graphviz))

#(define last-grob-action '())

#(define sym-blacklist
  '())

#(define sym-whitelist
  '())

#(define file-line-blacklist
  '())

#(define file-line-whitelist
  '())

#(define (blacklist-symbol sym)
  (set! sym-blacklist (cons sym sym-blacklist)))

#(define (whitelist-symbol sym)
  (set! sym-whitelist (cons sym sym-whitelist)))

#(define graph (make-graph "graph.dot"))

% an event is relevant if
% (it is on some whitelist or all whitelists are empty)
% and
% (it isn't on any blacklist)

#(define (relevant? grob file line prop)
  (let ((file-line `(,file . ,line)))
   (and
    (or
     (= 0 (length file-line-whitelist) (length sym-whitelist))
     (memq prop sym-whitelist)
     (member file-line file-line-whitelist))
    (and
     (not (memq prop sym-blacklist))
     (not (member file-line file-line-blacklist))))))

#(define (grob-event-node grob label cluster)
  (let ((node-id (add-node graph label cluster))
	(prev (assv grob last-grob-action)))
   (if (pair? prev)
       (add-edge graph (cdr prev) node-id))
   (set! last-grob-action (assv-set! last-grob-action grob node-id))))

#(define (truncate-value val)
  (let ((val-str (format "~a" val)))
   (string-take val-str (min 50 (string-length val-str)))))

#(define (grob-mod grob file line func prop val)
  (let* ((val-str (truncate-value val))
	 (label (format "~a\\n~a:~a\\n~a <- ~a" grob file line prop val-str)))
   (if (relevant? grob file line prop)
       (grob-event-node grob label file))))

#(define (grob-cache grob prop callback value)
  (let* ((val-str (truncate-value value))
	 (label (format "caching ~a.~a\\n~a -> ~a" grob prop callback value)))
   (if (relevant? grob #f #f prop)
       (grob-event-node grob label #f))))

#(define (grob-create grob file line func)
  (let ((label (format "~a\\n~a:~a" grob file line)))
   (grob-event-node grob label file)))

#(ly:set-grob-modification-callback grob-mod)
#(ly:set-property-cache-callback grob-cache)
%#(ly:set-grob-creation-callback grob-create)
