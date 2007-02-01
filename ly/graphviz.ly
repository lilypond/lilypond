\version "2.11.15"

#(define last-grob-action '())

#(define sym-blacklist
  '(cause font))

#(define sym-whitelist
  '(control-points))

#(define file-line-blacklist
  '())

#(define file-line-whitelist
  '())

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

#(define (grob-mod grob file line func prop val)
  (let* ((prev (assv grob last-grob-action))
         (val-str0 (format "~a" val))
         (val-str (string-take val-str0 (min 50 (string-length val-str0))))
	 (label (format "~a\\n~a:~a\\n~a <- ~a" grob file line prop val-str))
	 (node-id (make-node graph label file)))
   (if (relevant? grob file line prop)
    (begin
     (if (pair? prev)
      (add-edge graph (cdr prev) node-id))
     (set! last-grob-action (assv-set! last-grob-action grob node-id))))))

#(define (grob-create grob file line func)
  (let* ((label (format "~a\\n~a:~a" grob file line))
	 (node-id (make-node graph label file)))
   (set! last-grob-action (assv-set! last-grob-action grob node-num))))

#(ly:set-grob-modification-callback grob-mod)
#(ly:set-grob-creation-callback grob-create)
