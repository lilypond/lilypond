;;;; interface-description.scm -- part of generated backend documentation
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2001  Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>


; should include default value?

(ly-add-interface
'note-name-interface
 "Note name"
 '(style))

(ly-add-interface
 'dynamic-interface
   "Any kind of loudness sign"
   '()
    )

(ly-add-interface
 'finger-interface
 "A fingering instruction"
 '()
 )

(ly-add-interface
 'ligature-interface
 "A ligature"
 '()
 )

(ly-add-interface
 'mark-interface
 "a rehearsal mark"
 '(
   ))
