;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2023--2023 Jean Abou Samra <jean@abou-samra.fr>
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

"A minimal QR code encoder.

The goal is to remain simple, not to be optimal as a general-purpose
encoder.  For example, the QR code standard has several encoding modes
(digits only, alphanumeric with uppercase letters only, JIS, bytes)
but we always use bytes since the primary purpose of QR codes in music
scores is to encode URLs, and these usually aren't digits only, have
lowercase letters, and aren't in Japanese characters."

;; Here are some resources about QR code encoding:
;; https://observablehq.com/@zavierhenry/encoding-qr-codes
;; https://www.thonky.com/qr-code-tutorial
;; https://matchadesign.com/blog/qr-code-demystified

;; NB: #1@1(...) is syntax for an array with indices starting at 1.  See
;; "Array Syntax" in the Guile manual.

(define-module (lily qr-code))

(use-modules (rnrs bytevectors)
             (srfi srfi-1)
             (ice-9 match)
             ((ice-9 iconv) #:select (string->bytevector))
             (lily))

;; First some boring data

;; error correction level -> version -> number of bytes of information that can fit
(define capacity-table
  '((low . #1@1(17 32 53 78 106 134 154 192 230 271 321 367 425 458 520 586 644
                   718 792 858 929 1003 1091 1171 1273 1367 1465 1528 1628 1732
                   1840 1952 2068 2188 2303 2431 2563 2699 2809 2953))
    (medium . #1@1(14 26 42 62 84 106 122 152 180 213 251 287 331 362 412 450
                      504 560 624 666 711 779 857 911 997 1059 1125 1190 1264
                      1370 1452 1538 1628 1722 1809 1911 1989 2099 2213 2331))
    (quarter . #1@1(11 20 32 46 60 74 86 108 130 151 177 203 241 258 292 322
                       364 394 442 482 509 565 611 661 715 751 805 868 908 982
                       1030 1112 1168 1228 1283 1351 1423 1499 1579 1663))
    (high . #1@1(7 14 24 34 44 58 64 84 98 119 137 155 177 194 220 250 280 310
                   338 382 403 439 461 511 535 593 625 658 698 742 790 842 898
                   958 983 1051 1093 1139 1219 1273))))

;; error correction level -> version -> list of (count . size) pairs indicating
;; that the encoding uses `count` blocks of `size`
(define blocks-table
  '((low . #1@1(((1 . 19)) ((1 . 34)) ((1 . 55)) ((1 . 80)) ((1 . 108)) ((2 . 68))
                ((2 . 78)) ((2 . 97)) ((2 . 116)) ((2 . 68) (2 . 69)) ((4 . 81))
                ((2 . 92) (2 . 93)) ((4 . 107)) ((3 . 115) (1 . 116))
                ((5 . 87) (1 . 88)) ((5 . 98) (1 . 99)) ((1 . 107) (5 . 108))
                ((5 . 120) (1 . 121)) ((3 . 113) (4 . 114)) ((3 . 107) (5 . 108))
                ((4 . 116) (4 . 117)) ((2 . 111) (7 . 112)) ((4 . 121) (5 . 122))
                ((6 . 117) (4 . 118)) ((8 . 106) (4 . 107)) ((10 . 114) (2 . 115))
                ((8 . 122) (4 . 123)) ((3 . 117) (10 . 118)) ((7 . 116) (7 . 117))
                ((5 . 115) (10 . 116)) ((13 . 115) (3 . 116)) ((17 . 115))
                ((17 . 115) (1 . 116)) ((13 . 115) (6 . 116)) ((12 . 121) (7 . 122))
                ((6 . 121) (14 . 122)) ((17 . 122) (4 . 123)) ((4 . 122) (18 . 123))
                ((20 . 117) (4 . 118)) ((19 . 118) (6 . 119))))
    (medium . #1@1(((1 . 16)) ((1 . 28)) ((1 . 44)) ((2 . 32)) ((2 . 43)) ((4 . 27))
                   ((4 . 31)) ((2 . 38) (2 . 39)) ((3 . 36) (2 . 37)) ((4 . 43) (1 . 44))
                   ((1 . 50) (4 . 51)) ((6 . 36) (2 . 37)) ((8 . 37) (1 . 38))
                   ((4 . 40) (5 . 41)) ((5 . 41) (5 . 42)) ((7 . 45) (3 . 46))
                   ((10 . 46) (1 . 47)) ((9 . 43) (4 . 44)) ((3 . 44) (11 . 45))
                   ((3 . 41) (13 . 42)) ((17 . 42)) ((17 . 46)) ((4 . 47) (14 . 48))
                   ((6 . 45) (14 . 46)) ((8 . 47) (13 . 48)) ((19 . 46) (4 . 47))
                   ((22 . 45) (3 . 46)) ((3 . 45) (23 . 46)) ((21 . 45) (7 . 46))
                   ((19 . 47) (10 . 48)) ((2 . 46) (29 . 47)) ((10 . 46) (23 . 47))
                   ((14 . 46) (21 . 47)) ((14 . 46) (23 . 47)) ((12 . 47) (26 . 48))
                   ((6 . 47) (34 . 48)) ((29 . 46) (14 . 47)) ((13 . 46) (32 . 47))
                   ((40 . 47) (7 . 48)) ((18 . 47) (31 . 48))))
    (quarter . #1@1(((1 . 13)) ((1 . 22)) ((2 . 17)) ((2 . 24)) ((2 . 15) (2 . 16))
                    ((4 . 19)) ((2 . 14) (4 . 15)) ((4 . 18) (2 . 19)) ((4 . 16) (4 . 17))
                    ((6 . 19) (2 . 20)) ((4 . 22) (4 . 23)) ((4 . 20) (6 . 21))
                    ((8 . 20) (4 . 21)) ((11 . 16) (4 . 17)) ((5 . 24) (7 . 25))
                    ((15 . 19) (2 . 20)) ((1 . 22) (15 . 23)) ((17 . 22) (1 . 23))
                    ((17 . 21) (4 . 22)) ((15 . 24) (5 . 25)) ((17 . 22) (6 . 23))
                    ((7 . 24) (16 . 25)) ((11 . 24) (14 . 25)) ((11 . 24) (16 . 25))
                    ((7 . 24) (22 . 25)) ((28 . 22) (6 . 23)) ((8 . 23) (26 . 24))
                    ((4 . 24) (31 . 25)) ((1 . 23) (37 . 24)) ((15 . 24) (25 . 25))
                    ((42 . 24) (1 . 25)) ((10 . 24) (35 . 25)) ((29 . 24) (19 . 25))
                    ((44 . 24) (7 . 25)) ((39 . 24) (14 . 25)) ((46 . 24) (10 . 25))
                    ((49 . 24) (10 . 25)) ((48 . 24) (14 . 25)) ((43 . 24) (22 . 25))
                    ((34 . 24) (34 . 25))))
    (high . #1@1(((1 . 9)) ((1 . 16)) ((2 . 13)) ((4 . 9)) ((2 . 11) (2 . 12))
                 ((4 . 15)) ((4 . 13) (1 . 14)) ((4 . 14) (2 . 15)) ((4 . 12) (4 . 13))
                 ((6 . 15) (2 . 16)) ((3 . 12) (8 . 13)) ((7 . 14) (4 . 15))
                 ((12 . 11) (4 . 12)) ((11 . 12) (5 . 13)) ((11 . 12) (7 . 13))
                 ((3 . 15) (13 . 16)) ((2 . 14) (17 . 15)) ((2 . 14) (19 . 15))
                 ((9 . 13) (16 . 14)) ((15 . 15) (10 . 16)) ((19 . 16) (6 . 17))
                 ((34 . 13)) ((16 . 15) (14 . 16)) ((30 . 16) (2 . 17)) ((22 . 15) (13 . 16))
                 ((33 . 16) (4 . 17)) ((12 . 15) (28 . 16)) ((11 . 15) (31 . 16))
                 ((19 . 15) (26 . 16)) ((23 . 15) (25 . 16)) ((23 . 15) (28 . 16))
                 ((19 . 15) (35 . 16)) ((11 . 15) (46 . 16)) ((59 . 16) (1 . 17))
                 ((22 . 15) (41 . 16)) ((2 . 15) (64 . 16)) ((24 . 15) (46 . 16))
                 ((42 . 15) (32 . 16)) ((10 . 15) (67 . 16)) ((20 . 15) (61 . 16))))))

;; antilog table for GF(256)
(define gf256-antilog-table
  #1(1 2 4 8 16 32 64 128 29 58 116 232 205 135 19 38 76 152 45 90 180 117 234 201 143
       3 6 12 24 48 96 192 157 39 78 156 37 74 148 53 106 212 181 119 238 193 159 35
       70 140 5 10 20 40 80 160 93 186 105 210 185 111 222 161 95 190 97 194 153 47
       94 188 101 202 137 15 30 60 120 240 253 231 211 187 107 214 177 127 254 225
       223 163 91 182 113 226 217 175 67 134 17 34 68 136 13 26 52 104 208 189 103
       206 129 31 62 124 248 237 199 147 59 118 236 197 151 51 102 204 133 23 46 92
       184 109 218 169 79 158 33 66 132 21 42 84 168 77 154 41 82 164 85 170 73 146 57
       114 228 213 183 115 230 209 191 99 198 145 63 126 252 229 215 179 123 246 241
       255 227 219 171 75 150 49 98 196 149 55 110 220 165 87 174 65 130 25 50 100 200
       141 7 14 28 56 112 224 221 167 83 166 81 162 89 178 121 242 249 239 195 155 43
       86 172 69 138 9 18 36 72 144 61 122 244 245 247 243 251 235 203 139 11 22 44
       88 176 125 250 233 207 131 27 54 108 216 173 71 142))

;; log table for GF(256)
(define gf256-log-table
  #1@1(0 1 25 2 50 26 198 3 223 51 238 27 104 199 75 4 100 224 14 52 141 239 129 28
         193 105 248 200 8 76 113 5 138 101 47 225 36 15 33 53 147 142 218 240 18
         130 69 29 181 194 125 106 39 249 185 201 154 9 120 77 228 114 166 6 191
         139 98 102 221 48 253 226 152 37 179 16 145 34 136 54 208 148 206 143 150
         219 189 241 210 19 92 131 56 70 64 30 66 182 163 195 72 126 110 107 58 40 84
         250 133 186 61 202 94 155 159 10 21 121 43 78 212 229 172 115 243 167 87 7
         112 192 247 140 128 99 13 103 74 222 237 49 197 254 24 227 165 153 119 38
         184 180 124 17 68 146 217 35 32 137 46 55 63 209 91 149 188 207 205 144 135
         151 178 220 252 190 97 242 86 211 171 20 42 93 158 132 60 57 83 71 109 65 162
         31 45 67 216 183 123 164 118 196 23 73 236 127 12 111 246 108 161 59 82 41
         157 85 170 251 96 134 177 187 204 62 90 203 89 95 176 156 169 160 81 11 245 22
         235 122 117 44 215 79 174 213 233 230 231 173 232 116 214 244 234 168 80 88 175))

;; error correction level -> version -> number of error correction bytes per block
(define error-correction-bytes-per-block-table
  '((low . #1@1(7 10 15 20 26 18 20 24 30 18 20 24 26 30 22 24 28 30 28 28 28 28 30 30
                  26 28 30 30 30 30 30 30 30 30 30 30 30 30 30 30))
    (medium . #1@1(10 16 26 18 24 16 18 22 22 26 30 22 22 24 24 28 28 26 26 26 26 28
                      28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28))
    (quarter . #1@1(13 22 18 26 18 24 18 22 20 24 28 26 24 20 30 24 28 28 26 30 28
                       30 30 30 30 28 30 30 30 30 30 30 30 30 30 30 30 30 30 30))
    (high . #1@1(17 28 22 16 22 28 26 26 24 28 24 28 22 24 24 30 28 28 26 28 30 24
                    30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30))))

;; number of error correction bytes (per block) -> coefficients of error
;; correction polynomial
(define error-correction-polynomial-table
  '((7 . (1 127 122 154 164 11 68 117))
    (10 . (1 216 194 159 111 199 94 95 113 157 193))
    (13 . (1 137 73 227 17 177 17 52 13 46 43 83 132 120))
    (15 . (1 29 196 111 163 112 74 10 105 105 139 132 151 32 134 26))
    (16 . (1 59 13 104 189 68 209 30 8 163 65 41 229 98 50 36 59))
    (17 . (1 119 66 83 120 119 22 197 83 249 41 143 134 85 53 125 99 79))
    (18 . (1 239 251 183 113 149 175 199 215 240 220 73 82 173 75 32 67 217 146))
    (20 . (1 152 185 240 5 111 99 6 220 112 150 69 36 187 22 228 198 121 121 165 174))
    (22 . (1 89 179 131 176 182 244 19 189 69 40 28 137 29 123 67 253 86 218 230
             26 145 245))
    (24 . (1 122 118 169 70 178 237 216 102 115 150 229 73 130 72 61 43 206 1 237
             247 127 217 144 117))
    (26 . (1 246 51 183 4 136 98 199 152 77 56 206 24 145 40 209 117 233 42 135 68 70
             144 146 77 43 94))
    (28 . (1 252 9 28 13 18 251 208 150 103 174 100 41 167 12 247 56 117 119 233 127 181
             100 121 147 176 74 58 197))
    (30 . (1 212 246 77 73 195 192 75 98 5 70 103 177 22 217 138 51 181 246 72 25 18 46 228
             74 216 195 11 106 130 150))))

;; version -> version bits
(define version-bits-table
  #1@7(#b000111110010010100 #b001000010110111100 #b001001101010011001
                            #b001010010011010011 #b001011101111110110 #b001100011101100010
                            #b001101100001000111 #b001110011000001101 #b001111100100101000
                            #b010000101101111000 #b010001010001011101 #b010010101000010111
                            #b010011010100110010 #b010100100110100110 #b010101011010000011
                            #b010110100011001001 #b010111011111101100 #b011000111011000100
                            #b011001000111100001 #b011010111110101011 #b011011000010001110
                            #b011100110000011010 #b011101001100111111 #b011110110101110101
                            #b011111001001010000 #b100000100111010101 #b100001011011110000
                            #b100010100010111010 #b100011011110011111 #b100100101100001011
                            #b100101010000101110 #b100110101001100100 #b100111010101000001
                            #b101000110001101001))

;; error correction level -> mask number -> format bits
(define format-bits-table
  '((low . #(#b111011111000100 #b111001011110011 #b111110110101010 #b111100010011101 #b110011000101111
                               #b110001100011000 #b110110001000001 #b110100101110110))
    (medium . #(#b101010000010010 #b101000100100101 #b101111001111100 #b101101101001011 #b100010111111001
                                  #b100000011001110 #b100111110010111 #b100101010100000))
    (quarter . #(#b011010101011111 #b011000001101000 #b011111100110001 #b011101000000110 #b010010010110100
                                   #b010000110000011 #b010111011011010 #b010101111101101))
    (high . #(#b001011010001001 #b001001110111110 #b001110011100111 #b001100111010000
                                #b000011101100010 #b000001001010101 #b000110100001100 #b000100000111011))))

;; "finder pattern" put at three corners of the QR code
(define finder-pattern
  #2((#f #f #f #f #f #f #f #f)
     (#t #t #t #t #t #t #t #f)
     (#t #f #f #f #f #f #t #f)
     (#t #f #t #t #t #f #t #f)
     (#t #f #t #t #t #f #t #f)
     (#t #f #t #t #t #f #t #f)
     (#t #f #f #f #f #f #t #f)
     (#t #t #t #t #t #t #t #f)))

;; "alignment pattern" scattered through the grid
(define alignment-pattern
  #2((#t #t #t #t #t)
     (#t #f #f #f #t)
     (#t #f #t #f #t)
     (#t #f #f #f #t)
     (#t #t #t #t #t)))

;; version -> list of coordinates (both X and Y) of alignment patterns
(define alignment-pattern-locations-table
  #1@1(() (6 18) (6 22) (6 26) (6 30) (6 34) (6 22 38) (6 24 42) (6 26 46)
       (6 28 50) (6 30 54) (6 32 58) (6 34 62) (6 26 46 66) (6 26 48 70)
       (6 26 50 74) (6 30 54 78) (6 30 56 82) (6 30 58 86) (6 34 62 90)
       (6 28 50 72 94) (6 26 50 74 98) (6 30 54 78 102) (6 28 54 78 106)
       (6 32 58 84 110) (6 30 58 86 114) (6 34 62 90 118) (6 26 50 74 98 122)
       (6 30 54 78 102 126) (6 26 52 78 104 130) (6 30 56 82 108 134)
       (6 34 60 86 112 138) (6 30 58 86 114 142) (6 34 62 90 118 146)
       (6 30 54 78 102 126 150) (6 24 50 76 102 128 154) (6 28 54 80 106 132 158)
       (6 32 58 84 110 136 162) (6 26 54 82 110 138 166) (6 30 58 86 114 142 170)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here is the actual code:

(define (optimal-version needed-capacity correction-level)
  "Compute the optimal QR code version according to the number of bytes
to be stored and the desired error correction level."
  (let* ((capacity-per-version (assq-ref capacity-table correction-level))
         (max-capacity (array-ref capacity-per-version 40)))
    (when (> needed-capacity max-capacity)
      (ly:warning (G_ "too many bytes (~a) to fit in a QR code with \
correction level ~a (max ~a)")
                  needed-capacity correction-level max-capacity)
      (throw 'qr-code-error))
    (binary-search 1 40
                   (lambda (i) (array-ref capacity-per-version i))
                   needed-capacity
                   #:mode 'first-greater-than-or-equal)))

(define (data-payload bytevec version eventual-length)
  "Compute the information payload contained into the QR code, consisting
of the given bytevector, plus a mode marker and a length marker."
  (assert (zero? (modulo eventual-length 8)))
  (let* ((encoded (byte-list->bit-list (bytevector->u8-list bytevec)))
         (encoded-length (bytevector-length bytevec)) ; in bytes, not bits
         ;; In byte mode, QR codes with version 10 or higher use 2 bytes for the
         ;; character count indicator, other versions use 1 byte.
         (byte-count-length (if (< version 10) 8 16))
         (mode-indicator '(#f #t #f #f)) ; byte mode
         (byte-count-indicator (int->bit-list encoded-length byte-count-length))
         (data (append mode-indicator byte-count-indicator encoded))
         ;; Add the 4-bit zero "terminator" (or less than 4 bits if it would overflow)
         (terminator-length (min 4 (- eventual-length (length data))))
         (data-with-terminator (append data (make-list terminator-length #f)))
         ;; Add the extra padding to reach a byte boundary
         (remaining-length (- eventual-length (length data-with-terminator)))
         (alignment-padding-length (modulo remaining-length 8))
         (byte-aligned-data
          (append data-with-terminator (make-list alignment-padding-length #f)))
         ;; Pad on the right until capacity is reached using the two magic bytes
         ;; mandated by the spec
         (padded-data
          (list-pad-right byte-aligned-data eventual-length
                          #t #t #t #f #t #t #f #f #f #f #f #t #f #f #f #t)))
    padded-data))

;; addition, multiplication and inversion in GF(256)
(define +-gf256 logxor)
(define (*-gf256 a b)
  (if (or (zero? a) (zero? b))
      0
      (let* ((a-log (array-ref gf256-log-table a))
             (b-log (array-ref gf256-log-table b))
             (prod-log (modulo (+ a-log b-log) 255)))
        (array-ref gf256-antilog-table prod-log))))
(define (/-gf256 a)
  (assert (not (zero? a)))
  (let* ((a-log (array-ref gf256-log-table a))
         (inv-log (modulo (- a-log) 255)))
    (array-ref gf256-antilog-table inv-log)))

(define (reed-solomon basic-block correction-bytes)
  "Compute error correction bytes for a block."
  ;; View block as a polynomial in GF(256)[X], multiply it by X^correction-bytes,
  ;; then compute modulo by the error correction polynomial obtained from the table.
  (match-let* ((correction-poly
                (assv-ref error-correction-polynomial-table correction-bytes))
               ((correction-leading-coeff . _) correction-poly))
    (let loop ((block (append basic-block (make-list correction-bytes 0)))
               (block-degree (+ (1- (length basic-block)) correction-bytes)))
      (if (< block-degree correction-bytes)
          (begin
            (assert (eqv? correction-bytes (length block)))
            block)
          (match-let* (((block-leading-coeff . _) block)
                       (multiplier (*-gf256 block-leading-coeff
                                            (/-gf256 correction-leading-coeff))))
            (loop (let inner ((block block) (correction-poly correction-poly) (acc '()))
                    (match correction-poly
                      (()
                       (match-let (((0 . result) (reverse! acc block)))
                         result))
                      ((corr-coeff . corr-rest)
                       (match-let (((block-coeff . block-rest) block))
                         (inner block-rest corr-rest
                                (cons (+-gf256 block-coeff (*-gf256 multiplier corr-coeff))
                                      acc))))))
                  (1- block-degree)))))))

(define (error-correction payload version correction-level expanded-block-sizes)
  "Read the data payload, split it into blocks, compute error correction bytes
for each block, and interleave the results."
  (let* ((blocks
          (split-list-by-group-lengths (bit-list->byte-list payload)
                                       expanded-block-sizes))
         (correction-bytes
          (array-ref
           (assq-ref error-correction-bytes-per-block-table correction-level)
           version))
         (correction-blocks (map (lambda (block)
                                   (reed-solomon block correction-bytes))
                                 blocks))
         (all-blocks (append (apply flat-zip-longest blocks)
                             (apply flat-zip-longest correction-blocks))))
    (byte-list->bit-list all-blocks)))

(define (prefilled-qr-matrix version size correction-level mask-number)
  "Return a template for a QR code of the given version. The result
is a matrix, the elements of which can be booleans (for fixed patterns
mandated by the specification) or the @code{data} symbol (for a module,
i.e., a square, that is part of the QR code's data payload)."
  (let ((matrix (make-array 'data size size)))
    ;; Add "finder patterns"
    (let* ((finder-bottom-left finder-pattern)
           (finder-top-right (matrix-rotate-counterclockwise
                              (matrix-rotate-counterclockwise finder-bottom-left)))
           (finder-top-left (matrix-rotate-counterclockwise finder-top-right)))
      (array-copy/subarray! finder-top-left matrix 0 0)
      (array-copy/subarray! finder-bottom-left matrix (- size 8) 0)
      (array-copy/subarray! finder-top-right matrix 0 (- size 8)))
    ;; Add "alignment patterns"
    (let ((alignment-pattern-locations
           (array-ref alignment-pattern-locations-table version)))
      (for-each (lambda (row)
                  (for-each (lambda (column)
                              (unless (or (and (< row 10) (< column 10))
                                          (and (< row 10) (< (- size 1 column) 10))
                                          (and (< (- size 1 row) 10) (< column 10)))
                                (array-copy/subarray! alignment-pattern matrix
                                                      (- row 2) (- column 2))))
                            alignment-pattern-locations))
                alignment-pattern-locations))
    ;; Add "timing patterns"
    (for-each (lambda (i)
                (array-set! matrix (even? i) i 6)
                (array-set! matrix (even? i) 6 i))
              (iota (- size 16) 8))
    ;; Add the "dark module"
    (array-set! matrix #t (- size 8) 8)
    ;; Add the format information (correction level and mask number)
    (let ((format-bits
           (int->bit-list
            (array-ref (assq-ref format-bits-table correction-level)
                       mask-number)
            15)))
      (define (add-format-bits position-list)
        (for-each (match-lambda*
                    (((row . column) bit)
                     (array-set! matrix bit row column)))
                  position-list format-bits))
      (add-format-bits '((8 . 0) (8 . 1) (8 . 2) (8 . 3) (8 . 4) (8 . 5)
                         (8 . 7) (8 . 8) (7 . 8) (5 . 8) (4 . 8) (3 . 8)
                         (2 . 8) (1 . 8) (0 . 8)))
      (add-format-bits `((,(- size 1) . 8) (,(- size 2) . 8) (,(- size 3) . 8)
                         (,(- size 4) . 8) (,(- size 5) . 8) (,(- size 6) . 8)
                         (,(- size 7) . 8) (8 . ,(- size 8)) (8 . ,(- size 7))
                         (8 . ,(- size 6)) (8 . ,(- size 5)) (8 . ,(- size 4))
                         (8 . ,(- size 3)) (8 . ,(- size 2)) (8 . ,(- size 1)))))
    ;; Add the version marker (only for versions >= 7)
    (when (>= version 7)
      (let* ((version-bits
              (reverse (int->bit-list
                        (array-ref version-bits-table version) 18)))
             (version-block (make-array 'dummy 6 3)))
        (array-index-map! version-block
                          (lambda (_row _column)
                            (match-let (((bit . rest) version-bits))
                              (set! version-bits rest)
                              bit)))
        (array-copy/subarray! version-block matrix 0 (- size 11))
        (let ((transposed (transpose-array version-block 1 0)))
          (array-copy/subarray! transposed matrix (- size 11) 0))))
    matrix))

(define all-masks
  (vector (lambda (row column) (even? (+ row column)))
          (lambda (row column) (even? row))
          (lambda (row column) (zero? (modulo column 3)))
          (lambda (row column) (zero? (modulo (+ row column) 3)))
          (lambda (row column) (even? (+ (floor (/ row 2)) (floor (/ column 3)))))
          (lambda (row column) (zero? (modulo (* row column) 6)))
          (lambda (row column) (even? (+ (modulo (* row column) 2) (modulo (* row column) 3))))
          (lambda (row column) (even? (+ (modulo (+ row column) 2) (modulo (* row column) 3))))))

(define (add-payload-with-mask! matrix size payload-with-error-correction mask)
  (let ((direction UP)
        (row (1- size))
        (column (1- size))
        (parity #t)
        (bits (append payload-with-error-correction
                      ;; remainder bits
                      (circular-list #f))))
    (let loop ()
      (match bits
        ((bit . rest)
         (when (eq? 'data (array-ref matrix row column))
           (set! bits rest)
           (let* ((mask-result (mask row column))
                  (bit (if mask-result (not bit) bit)))
             (array-set! matrix bit row column)))
         (cond
          ((eqv? column 6)
           ;; vertical timing pattern must be skipped
           (set! column 5)
           (set! parity (not parity)))
          ((and (eqv? row 0) (eqv? direction UP) (not parity))
           (set! column (1- column))
           (set! direction DOWN))
          ((and (eqv? row (1- size)) (eqv? direction DOWN) (not parity))
           (set! column (1- column))
           (set! direction UP))
          (parity
           (set! column (1- column)))
          (else
           (set! row (- row direction))
           (set! column (1+ column))))
         (set! parity (not parity))
         (unless (and (eqv? row (1- size)) (eqv? column 0))
           (loop)))))))

(define (qr-encode-with-mask bytevec correction-level mask-number)
  (let* (;; Step 1: determine optimal code version for the length of the data
         (version (optimal-version (bytevector-length bytevec) correction-level))
         (size (+ 17 (* 4 version)))
         ;; Get the payload block sizes for this version (and correction level)
         (block-sizes (array-ref (assq-ref blocks-table correction-level) version))
         (expanded-block-sizes (append-map (match-lambda ((count . len)
                                                          (make-list count len)))
                                           block-sizes))
         (eventual-payload-length (* 8 (apply + expanded-block-sizes)))
         (payload (data-payload bytevec version eventual-payload-length))
         (payload-with-error-correction
          (error-correction payload version correction-level expanded-block-sizes))
         (prefilled (prefilled-qr-matrix version size correction-level mask-number))
         (mask (vector-ref all-masks mask-number)))
    (add-payload-with-mask! prefilled size payload-with-error-correction mask)
    prefilled))

(define (score-matrix matrix)
  (define size (car (array-dimensions matrix)))
  (define transposed-matrix (transpose-array matrix 1 0))

  (define (score-all-rows maybe-transposed-matrix scorer)
    "Sum the results of @var{scorer} over all rows of @var{maybe-transposed-matrix}."
    (let ((score 0))
      (for-each
       (lambda (i)
         (set! score
               (+ score (scorer (array-cell-ref maybe-transposed-matrix i)))))
       (iota size))
      score))

  (define (score-all-rows-and-columns scorer)
    "Sum the results of @var{scorer} over all rows and columns of @var{matrix}."
    (+ (score-all-rows matrix scorer)
       (score-all-rows transposed-matrix scorer)))

  ;; Penalty for too many consecutive modules (small squares) with the same color.
  (define (score-consecutive-same-color-modules row)
    (let loop ((score 0) (i 0))
      (if (eqv? i size)
          score
          ;; Find the longest monochromatic group starting from i
          (let* ((current (array-ref row i))
                 (next-different (let find-next-different ((j (1+ i)))
                                   (if (or (eqv? j size)
                                           (not (eq? current (array-ref row j))))
                                       j
                                       (find-next-different (1+ j)))))
                 (group-length (- next-different i)))
            ;; Penalty is 3 for 5 consecutive same-color modules, and 1 more per
            ;; additional module
            (loop (if (<= 5 group-length)
                      (+ score group-length -2)
                      score)
                  next-different)))))

  ;; Penalty for every 2x2 square that has 4 modules of the same color
  (define (score-2x2-areas-same-color)
    (let ((score 0))
      (for-each
       (lambda (i)
         (for-each
          (lambda (j)
            (when (eq? (array-ref matrix i j)
                       (array-ref matrix (1+ i) j)
                       (array-ref matrix i (1+ j))
                       (array-ref matrix (1+ i) (1+ j)))
              (set! score (+ score 3))))
          (iota (1- size))))
       (iota (1- size)))
      score))

  ;; Penalty for certain special patterns (too similar to the finder patterns)
  (define (score-special-patterns row)
    (let loop ((score 0) (i 0))
      (if (eqv? i (- size 10))
          score
          (loop (if (any (lambda (pat)
                           (let inner ((j i) (pat pat))
                             (match pat
                               (()
                                #t)
                               ((bool . rest)
                                (and (eq? bool (array-ref row j))
                                     (inner (1+ j) rest))))))
                         ;; search for these two special patterns
                         '((#t #f #t #t #t #f #t #f #f #f #f)
                           (#f #f #f #f #t #f #t #t #t #f #t)))
                    ;; penalty of 40 whenever such a pattern is found
                    (+ score 40)
                    score)
                (1+ i)))))

  (define (score-dark-white-balance)
    (let ((dark-count 0))
      (array-for-each
       (lambda (b)
         (when b
           (set! dark-count (1+ dark-count))))
       matrix)
      (let* ((ratio (/ dark-count (expt size 2)))
             (imbalance (abs (- 1/2 ratio))))
        (* 10 (floor (* imbalance 20))))))

  (let ((score1 (score-all-rows-and-columns score-consecutive-same-color-modules))
        (score2 (score-2x2-areas-same-color))
        (score3 (score-all-rows-and-columns score-special-patterns))
        (score4 (score-dark-white-balance)))
    (+ score1 score2 score3 score4)))

(define-public (qr-encode str correction-level)
  "Encode the given bytevector into a QR code, returning a matrix (2-dimensional
array) of booleans.

A decoder will interpret the contents of the bytevector as a latin1 string.

On failure, report a warning and throw an exception with key @code{qr-code-error}."
  (unless (memq correction-level '(low medium quarter high))
    (ly:warning
     (G_ "QR code error correction level must be 'low, 'medium, 'quarter or \
'high; found ~s")
     correction-level)
    (throw 'qr-code-error))
  ;; There apparently exists an "ECI mode" (Extended Character Interpretation)
  ;; where strings can be encoded in UTF-8, but it's more complex and it seems
  ;; not all decoders support it.  Non-ASCII characters are uncommon in URLs,
  ;; so encoding in latin1 and gracefully refusing input that can't be encoded
  ;; in latin1 should be good enough.
  (let* ((bytevec (catch 'encoding-error
                         (lambda ()
                           (string->bytevector str "latin1"))
                         (lambda (key . _)
                           (ly:warning (G_ "string ~s cannot be \
represented in a QR code due to special characters") str)
                           (throw 'qr-code-error))))
         (possibilities
          (map (lambda (mask-number)
                 (let ((matrix (qr-encode-with-mask bytevec correction-level mask-number)))
                   (cons matrix (score-matrix matrix))))
               (iota 8))))
    (car (apply minmax/cmp (comparator-from-key cdr <) possibilities))))
