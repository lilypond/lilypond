; Music vocabularies are Copyright (C) 1993 Free Software Foundation, Inc.
; Francois Pinard <pinard@iro.umontreal.ca>,
; Neil Jerram <nj104@cus.cam.ac.uk>.
; Forms by Han-Wen Nienhuys

(setq forms-file "glossary-table.data")
(setq forms-number-of-fields 7)
(setq forms-read-only nil)                 ; to make sure
(setq forms-field-sep "@")
(setq forms-multi-line "\C-m")

(setq forms-format-list
      (list
       "*** Musical vocabulary ***\n"
       "\nItaliano:          " 6
       "\nFrancais:          " 2
       "\nUS English:        " 1
       "\nDeutsch:           " 3
       "\nBritish English:   " 4
       "\nNederlands:        " 5
       "\n\nExplanation\n\n" 7

	))
