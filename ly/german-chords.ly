BABYL OPTIONS: -*- rmail -*-
Version: 5
Labels:
Note:   This is the header of an rmail file.
Note:   If you are seeing it in rmail,
Note:    it means the file has no messages in it.

1,,
Return-Path: rz@daimi.au.dk
Delivery-Date: Fri, 13 Apr 2001 13:33:44 +0200
Received: from localhost (localhost [127.0.0.1])
	by appel.lilypond.org (8.9.3/8.9.3/Debian 8.9.3-21) with ESMTP id NAA02351
	for <fred@localhost>; Fri, 13 Apr 2001 13:33:43 +0200
X-XS4ALL-To: <jantien@xs4all.nl>
Received: from pop.xs4all.nl
	by localhost with POP3 (fetchmail-5.1.2)
	for fred@localhost (single-drop); Fri, 13 Apr 2001 13:33:44 +0200 (CEST)
Received: from smtp5.xs4all.nl (smtp5.xs4all.nl [194.109.6.49])
	by maildrop7.xs4all.nl (8.11.1/8.11.1) with ESMTP id f3DBWCd93235
	for <jantien@xs4all.nl>; Fri, 13 Apr 2001 13:32:12 +0200 (CEST)
	(envelope-from rz@daimi.au.dk)
Received: from fencepost.gnu.org (fencepost.gnu.org [199.232.76.164])
	by smtp5.xs4all.nl (8.9.3/8.9.3) with ESMTP id NAA10302
	for <jantien@xs4all.nl>; Fri, 13 Apr 2001 13:31:04 +0200 (CEST)
Received: from nightcrawler.daimi.au.dk ([130.225.18.95])
	by fencepost.gnu.org with esmtp (Exim 3.16 #1 (Debian))
	id 14o1n2-0008Pp-00
	for <janneke@gnu.org>; Fri, 13 Apr 2001 07:31:00 -0400
Received: from daimi.au.dk (localhost [127.0.0.1])
	by nightcrawler.daimi.au.dk (8.11.2/8.11.2) with ESMTP id f3DBUwX31324
	for <janneke@gnu.org>; Fri, 13 Apr 2001 13:30:58 +0200
Sender: rz@daimi.au.dk
Message-ID: <3AD6E372.640DEE61@daimi.au.dk>
Date: Fri, 13 Apr 2001 13:30:58 +0200
From: Rune Zedeler <rz@daimi.au.dk>
X-Mailer: Mozilla 4.76 [en] (X11; U; Linux 2.2.16-3 i686)
X-Accept-Language: en
MIME-Version: 1.0
To: Jan Nieuwenhuizen <janneke@gnu.org>
Subject: german-chords
Content-Type: multipart/mixed;
 boundary="------------69CC07F348020448DD291CA8"
X-UIDL: 987161532.maildrop7.93237

*** EOOH ***
\version "1.3.148"

%  german-chords.ly:
% german/norwegian/danish?

% To get Bb instead of B, use
% \include "german-chords.ly"
% #(set! german-Bb #t)

#(define german-Bb #f)

#(define (pitch->chord-name-text-banter pitch steps)
   (let ((dopitch (if (member (cdr pitch) '((6 -1) (6 -2)))
		      (list 7 (+ (if german-Bb 0 1) (caddr pitch)))
		      (cdr pitch)
		 )))
     (cons
       (list-ref '("C" "D" "E" "F" "G" "A" "H" "B") (car dopitch))
       (accidental->text-super (cadr dopitch))
     )
   )
 )



#(define (pitch->note-name-text-banter pitch)
   (let ((dopitch (if (member (cdr pitch) '((6 -1) (6 -2)))
		     (list 7 (+ 1 (caddr pitch)))
		     (cdr pitch)
		 )))
     (list
       (string-append
	  (list-ref '("c" "d" "e" "f" "g" "a" "h" "b") (car dopitch))
	  (if (or (equal? (car dopitch) 2) (equal? (car dopitch) 5))
	    (list-ref '( "ses"  "s" "" "is" "isis") (+ 2 (cadr dopitch)))
	    (list-ref '("eses" "es" "" "is" "isis") (+ 2 (cadr dopitch)))
	  )
       )
     )
   )
 )

