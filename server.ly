
\paper
{
  raggedright = ##t
  indent = 0.0 
}

\layout {
  \context {
    \Score
    \override BarNumber  #'break-visibility = #all-visible
  }
  \context {
    \Voice
    \remove "Note_heads_engraver"
    \consists "Completion_heads_engraver"
  }
}

#(define (render-socket-music music socket)
  (let*
   ((score (ly:make-score music)) 
    )

   (ly:score-process score #f $defaultpaper $defaultlayout socket)
  ))



#(if #t
  (let ((s (socket PF_INET SOCK_STREAM 0)))
       (setsockopt s SOL_SOCKET SO_REUSEADDR 1)
       ;; Specific address?
       ;; (bind s AF_INET (inet-aton "127.0.0.1") 2904)
       (bind s AF_INET INADDR_ANY 2904)
       (listen s 5)

       (simple-format #t "Listening for clients in pid: ~S" (getpid))
       (newline)

       (while #t
              (let* ((client-connection (accept s))
		     (start-time (get-internal-real-time))
                     (client-details (cdr client-connection))
                     (client (car client-connection)))
                (simple-format #t "Got new client connection: ~S"
                               client-details)
                (newline)
                (simple-format #t "Client address: ~S"
                               (gethostbyaddr
                                (sockaddr:addr client-details)))
                (newline)
                ;; Send back the greeting to the client port
                (display (format
			  "hello LilyPond ~a\n"
			  (lilypond-version))
			  client)

	       (let* ((question (read client))
		      (music (eval question (current-module))))

		(render-socket-music music client)
                (close client)
		(display (format "Finished. Time elapsed: ~a\n"
			  (/ (- (get-internal-real-time) start-time) (* 1.0 internal-time-units-per-second))
			  
			))
		(gc) ; do GC while app is refreshing the screen.
	      )))))


#(define test-exp (make-music
  'SequentialMusic
  'elements
  (list (make-music
          'EventChord
          'elements
          (list (make-music
                  'NoteEvent
		  'input-tag 42
                  'duration
                  (ly:make-duration 2 0 1 1)
                  'pitch
                  (ly:make-pitch -1 0 0))))))
)

#(render-socket-music test-exp "test")
  

