; Computer vocabularies are Copyright (C) 1998 
; Jan Nieuwenhuizen <janneke@gnu.org>
; Han-Wen Nienhuys <hanwen@cs.uu.nl>

(setq forms-file "computer.data")
(setq forms-number-of-fields 7)
(setq forms-read-only nil)                 ; to make sure
(setq forms-field-sep ":")
(setq forms-multi-line nil)

(setq forms-format-list
      (list
       "*** Musical vocabulary ***\n"
       "\nUS English:        " 1
       "\nFrancais:          " 2
       "\nDeutsch:           " 3
       "\nBritish English:   " 4
       "\nNederlands:        " 5
       "\nItaliano:          " 6
       "\n\nExplanation\n\n" 7
	))
