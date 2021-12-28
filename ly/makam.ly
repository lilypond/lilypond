%%%% Makam note names.
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2006--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
%%%%
%%%% LilyPond is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%%
%%%% LilyPond is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%%
%%%% You should have received a copy of the GNU General Public License
%%%% along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

\version "2.23.3"

%{

Define 1/9 alterations.

%}


#(define-public EKSIK-IKI 5/18)
#(define-public EKSIK-UC 6/18)

#(define-public KOMA 1/9)
#(define-public BAKIYE 4/9)
#(define-public KUCUK 5/9)
#(define-public BUYUKMUCENNEB 8/9)

%{

Define pitch names

%}

makamPitchNames = #`(
  (c . ,(ly:make-pitch -1 0 NATURAL))
  (d . ,(ly:make-pitch -1 1 NATURAL))
  (e . ,(ly:make-pitch -1 2 NATURAL))
  (f . ,(ly:make-pitch -1 3 NATURAL))
  (g . ,(ly:make-pitch -1 4 NATURAL))
  (a . ,(ly:make-pitch -1 5 NATURAL))
  (b . ,(ly:make-pitch -1 6 NATURAL))

  (cc . ,(ly:make-pitch -1 0 KOMA))
  (dc . ,(ly:make-pitch -1 1 KOMA))
  (ec . ,(ly:make-pitch -1 2 KOMA))
  (fc . ,(ly:make-pitch -1 3 KOMA))
  (gc . ,(ly:make-pitch -1 4 KOMA))
  (ac . ,(ly:make-pitch -1 5 KOMA))
  (bc . ,(ly:make-pitch -1 6 KOMA))

  (cb . ,(ly:make-pitch -1 0 BAKIYE))
  (db . ,(ly:make-pitch -1 1 BAKIYE))
  (eb . ,(ly:make-pitch -1 2 BAKIYE))
  (fb . ,(ly:make-pitch -1 3 BAKIYE))
  (gb . ,(ly:make-pitch -1 4 BAKIYE))
  (ab . ,(ly:make-pitch -1 5 BAKIYE))
  (bb . ,(ly:make-pitch -1 6 BAKIYE))

  (ck . ,(ly:make-pitch -1 0 KUCUK))
  (dk . ,(ly:make-pitch -1 1 KUCUK))
  (ek . ,(ly:make-pitch -1 2 KUCUK))
  (fk . ,(ly:make-pitch -1 3 KUCUK))
  (gk . ,(ly:make-pitch -1 4 KUCUK))
  (ak . ,(ly:make-pitch -1 5 KUCUK))
  (bk . ,(ly:make-pitch -1 6 KUCUK))

  (cbm . ,(ly:make-pitch -1 0 BUYUKMUCENNEB))
  (dbm . ,(ly:make-pitch -1 1 BUYUKMUCENNEB))
  (ebm . ,(ly:make-pitch -1 2 BUYUKMUCENNEB))
  (fbm . ,(ly:make-pitch -1 3 BUYUKMUCENNEB))
  (gbm . ,(ly:make-pitch -1 4 BUYUKMUCENNEB))
  (abm . ,(ly:make-pitch -1 5 BUYUKMUCENNEB))
  (bbm . ,(ly:make-pitch -1 6 BUYUKMUCENNEB))

  ;; f for flat.
  (cfc . ,(ly:make-pitch -1 0 (- KOMA)))
  (dfc . ,(ly:make-pitch -1 1 (- KOMA)))
  (efc . ,(ly:make-pitch -1 2 (- KOMA)))
  (ffc . ,(ly:make-pitch -1 3 (- KOMA)))
  (gfc . ,(ly:make-pitch -1 4 (- KOMA)))
  (afc . ,(ly:make-pitch -1 5 (- KOMA)))
  (bfc . ,(ly:make-pitch -1 6 (- KOMA)))

  (cfb . ,(ly:make-pitch -1 0 (- BAKIYE)))
  (dfb . ,(ly:make-pitch -1 1 (- BAKIYE)))
  (efb . ,(ly:make-pitch -1 2 (- BAKIYE)))
  (ffb . ,(ly:make-pitch -1 3 (- BAKIYE)))
  (gfb . ,(ly:make-pitch -1 4 (- BAKIYE)))
  (afb . ,(ly:make-pitch -1 5 (- BAKIYE)))
  (bfb . ,(ly:make-pitch -1 6 (- BAKIYE)))

  (cfk . ,(ly:make-pitch -1 0 (- KUCUK)))
  (dfk . ,(ly:make-pitch -1 1 (- KUCUK)))
  (efk . ,(ly:make-pitch -1 2 (- KUCUK)))
  (ffk . ,(ly:make-pitch -1 3 (- KUCUK)))
  (gfk . ,(ly:make-pitch -1 4 (- KUCUK)))
  (afk . ,(ly:make-pitch -1 5 (- KUCUK)))
  (bfk . ,(ly:make-pitch -1 6 (- KUCUK)))

  (cfi . ,(ly:make-pitch -1 0 (- EKSIK-IKI)))
  (dfi . ,(ly:make-pitch -1 1 (- EKSIK-IKI)))
  (efi . ,(ly:make-pitch -1 2 (- EKSIK-IKI)))
  (ffi . ,(ly:make-pitch -1 3 (- EKSIK-IKI)))
  (gfi . ,(ly:make-pitch -1 4 (- EKSIK-IKI)))
  (afi . ,(ly:make-pitch -1 5 (- EKSIK-IKI)))
  (bfi . ,(ly:make-pitch -1 6 (- EKSIK-IKI)))

  (cfu . ,(ly:make-pitch -1 0 (- EKSIK-UC)))
  (dfu . ,(ly:make-pitch -1 1 (- EKSIK-UC)))
  (efu . ,(ly:make-pitch -1 2 (- EKSIK-UC)))
  (ffu . ,(ly:make-pitch -1 3 (- EKSIK-UC)))
  (gfu . ,(ly:make-pitch -1 4 (- EKSIK-UC)))
  (afu . ,(ly:make-pitch -1 5 (- EKSIK-UC)))
  (bfu . ,(ly:make-pitch -1 6 (- EKSIK-UC)))


  (cfbm . ,(ly:make-pitch -1 0 (- BUYUKMUCENNEB)))
  (dfbm . ,(ly:make-pitch -1 1 (- BUYUKMUCENNEB)))
  (efbm . ,(ly:make-pitch -1 2 (- BUYUKMUCENNEB)))
  (ffbm . ,(ly:make-pitch -1 3 (- BUYUKMUCENNEB)))
  (gfbm . ,(ly:make-pitch -1 4 (- BUYUKMUCENNEB)))
  (afbm . ,(ly:make-pitch -1 5 (- BUYUKMUCENNEB)))
  (bfbm . ,(ly:make-pitch -1 6 (- BUYUKMUCENNEB)))

)


%% set pitch names.
#(set! language-pitch-names
       (append language-pitch-names
               (list `(makam . ,makamPitchNames))))
\language "makam"

#(define eksikMirroredSlashedFlat
  (if (defined? 'eksikMirroredSlashedFlat)
       eksikMirroredSlashedFlat #f))

makamGlyphs = #`((1 . "accidentals.doublesharp")
       (8/9 . "accidentals.sharp.slashslashslash.stemstem")
       (5/9 . "accidentals.sharp.slashslashslash.stem")
       (4/9 . "accidentals.sharp")
       (1/9 . "accidentals.sharp.slashslash.stem")
       (0 . "accidentals.natural")
       (-1/9 . "accidentals.mirroredflat")
       (-5/18 . ,(if eksikMirroredSlashedFlat
                 "accidentals.mirroredflat.backslash"
                 "accidentals.mirroredflat"))
       (-6/18 . ,(if eksikMirroredSlashedFlat
                 "accidentals.mirroredflat.backslash"
                 "accidentals.mirroredflat"))
       (-4/9 . "accidentals.flat.slash")
       (-5/9 . "accidentals.flat")
       (-8/9 . "accidentals.flat.slashslash")
       (-1 . "accidentals.flatflat"))

\paper {
  font-defaults.alteration-glyph-name-alist = \makamGlyphs
}
