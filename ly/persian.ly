%%%% Definitions for writing Persian classic music scores
%%%%
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2009-2021 Kees van den Doel  <kvandoel@shaw.ca>
%%%% Copyright (C) 2021--2022 Werner Lemberg  <wl@gnu.org>
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

\version "2.23.6"


% Define pitch names.

#(define-public KORON -3/10)
#(define-public SORI 1/5)
#(define-public VLAT -1/10)
#(define-public FVLAT -3/5)
#(define-public SVLAT 2/5)

persianPitchNames = #`(
  (cff . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
  (cf . ,(ly:make-pitch -1 0 FLAT))
  (cv . ,(ly:make-pitch -1 0 VLAT))
  (cfv . ,(ly:make-pitch -1 0 FVLAT))
  (ck . ,(ly:make-pitch -1 0 KORON))
  (c . ,(ly:make-pitch -1 0 NATURAL))
  (co . ,(ly:make-pitch -1 0 SORI))
  (cs . ,(ly:make-pitch -1 0 SHARP))
  (csv . ,(ly:make-pitch -1 0 SVLAT))
  (css . ,(ly:make-pitch -1 0 DOUBLE-SHARP))
  (cx . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

  (dff . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
  (df . ,(ly:make-pitch -1 1 FLAT))
  (dv . ,(ly:make-pitch -1 1 VLAT))
  (dfv . ,(ly:make-pitch -1 1 FVLAT))
  (dk . ,(ly:make-pitch -1 1 KORON))
  (d . ,(ly:make-pitch -1 1 NATURAL))
  (do . ,(ly:make-pitch -1 1 SORI))
  (ds . ,(ly:make-pitch -1 1 SHARP))
  (dsv . ,(ly:make-pitch -1 1 SVLAT))
  (dss . ,(ly:make-pitch -1 1 DOUBLE-SHARP))
  (dx . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

  (eff . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
  (ef . ,(ly:make-pitch -1 2 FLAT))
  (ev . ,(ly:make-pitch -1 2 VLAT))
  (efv . ,(ly:make-pitch -1 2 FVLAT))
  (ek . ,(ly:make-pitch -1 2 KORON))
  (e . ,(ly:make-pitch -1 2 NATURAL))
  (eo . ,(ly:make-pitch -1 2 SORI))
  (es . ,(ly:make-pitch -1 2 SHARP))
  (esv . ,(ly:make-pitch -1 2 SVLAT))
  (ess . ,(ly:make-pitch -1 2 DOUBLE-SHARP))
  (ex . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

  (fff . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
  (ff . ,(ly:make-pitch -1 3 FLAT))
  (fv . ,(ly:make-pitch -1 3 VLAT))
  (ffv . ,(ly:make-pitch -1 3 FVLAT))
  (fk . ,(ly:make-pitch -1 3 KORON))
  (f . ,(ly:make-pitch -1 3 NATURAL))
  (fo . ,(ly:make-pitch -1 3 SORI))
  (fs . ,(ly:make-pitch -1 3 SHARP))
  (fsv . ,(ly:make-pitch -1 3 SVLAT))
  (fss . ,(ly:make-pitch -1 3 DOUBLE-SHARP))
  (fx . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

  (gff . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
  (gf . ,(ly:make-pitch -1 4 FLAT))
  (gv . ,(ly:make-pitch -1 4 VLAT))
  (gfv . ,(ly:make-pitch -1 4 FVLAT))
  (gk . ,(ly:make-pitch -1 4 KORON))
  (g . ,(ly:make-pitch -1 4 NATURAL))
  (go . ,(ly:make-pitch -1 4 SORI))
  (gs . ,(ly:make-pitch -1 4 SHARP))
  (gsv . ,(ly:make-pitch -1 4 SVLAT))
  (gss . ,(ly:make-pitch -1 4 DOUBLE-SHARP))
  (gx . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

  (aff . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
  (af . ,(ly:make-pitch -1 5 FLAT))
  (av . ,(ly:make-pitch -1 5 VLAT))
  (afv . ,(ly:make-pitch -1 5 FVLAT))
  (ak . ,(ly:make-pitch -1 5 KORON))
  (a . ,(ly:make-pitch -1 5 NATURAL))
  (ao . ,(ly:make-pitch -1 5 SORI))
  (as . ,(ly:make-pitch -1 5 SHARP))
  (asv . ,(ly:make-pitch -1 5 SVLAT))
  (ass . ,(ly:make-pitch -1 5 DOUBLE-SHARP))
  (ax . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

  (bff . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
  (bf . ,(ly:make-pitch -1 6 FLAT))
  (bv . ,(ly:make-pitch -1 6 VLAT))
  (bfv . ,(ly:make-pitch -1 6 FVLAT))
  (bk . ,(ly:make-pitch -1 6 KORON))
  (b . ,(ly:make-pitch -1 6 NATURAL))
  (bo . ,(ly:make-pitch -1 6 SORI))
  (bs . ,(ly:make-pitch -1 6 SHARP))
  (bsv . ,(ly:make-pitch -1 6 SVLAT))
  (bss . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
  (bx . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
)

% Register pitch names.

#(set! language-pitch-names
       (append language-pitch-names
               (list `(persian . ,persianPitchNames))))
\language "persian"


% Define accidental symbols.

persianGlyphs = #`(
  (-3/10 . "accidentals.flat.koron")
  (1/5 . "accidentals.sharp.sori")
  (0 . "accidentals.natural")
  (1/2 . "accidentals.sharp")
  (2/5 . "accidentals.sharp")
  (-1/2 . "accidentals.flat")
  (-3/5 . "accidentals.flat")
  (-1/10 . "")
  (-3/5 . "")
  (-1 . "accidentals.flatflat")
  (1 . "accidentals.doublesharp")
)

% Register accidental symbols.

\paper {
  font-defaults.alteration-glyph-name-alist = \persianGlyphs
}

\layout {
  \context {
    \Staff
    extraNatural = ##f
  }
}


% Define Persian tunings.

shur = #`(
  (0 . ,NATURAL)
  (1 . ,KORON)
  (2 . ,FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,FLAT)
)

% An alternative if you want to use shur on G with Bb vlat (bfv)
% instead of normal Bb (bf)
shurGvlat = #`(
  (0 . ,NATURAL)
  (1 . ,KORON)
  (2 . ,FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,KORON)
  (6 . ,FVLAT)
)

shurk = #`(
  (0 . ,NATURAL)
  (1 . ,KORON)
  (2 . ,FLAT)
  (3 . ,NATURAL)
  (4 . ,KORON)
  (5 . ,FLAT)
  (6 . ,FLAT)
)

esfahan = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,KORON)
  (6 . ,NATURAL)
)

mokhalefsegah = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,KORON)
  (6 . ,KORON)
)

chahargah = #`(
  (0 . ,NATURAL)
  (1 . ,KORON)
  (2 . ,NATURAL)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,KORON)
  (6 . ,NATURAL)
)

mahur = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,NATURAL)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,NATURAL)
  (6 . ,NATURAL)
)

delkashMahur = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,NATURAL)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,KORON)
  (6 . ,FLAT)
)


% Define key alteration orders.

persianKeyAlterationOrder = #`(
  (6 . ,FLAT) (2 . ,FLAT)
  (5 . ,FLAT) (1 . ,FLAT)
  (4 . ,FLAT) (0 . ,FLAT) (3 . ,FLAT)

  (6 . ,KORON) (2 . ,KORON)
  (5 . ,KORON) (1 . ,KORON)
  (4 . ,KORON) (0 . ,KORON) (3 . ,KORON)

  (3 . ,SHARP) (0 . ,SHARP)
  (4 . ,SHARP) (1 . ,SHARP)
  (5 . ,SHARP) (2 . ,SHARP) (6 . ,SHARP)

  (3 . ,SORI) (0 . ,SORI)
  (4 . ,SORI) (1 . ,SORI)
  (5 . ,SORI) (2 . ,SORI) (6 . ,SORI)

  (6 . ,DOUBLE-FLAT) (2 . ,DOUBLE-FLAT)
  (5 . ,DOUBLE-FLAT) (1 . ,DOUBLE-FLAT)
  (4 . ,DOUBLE-FLAT) (0 . ,DOUBLE-FLAT) (3 . ,DOUBLE-FLAT)

  (3 . ,DOUBLE-SHARP) (0 . ,DOUBLE-SHARP)
  (4 . ,DOUBLE-SHARP) (1 . ,DOUBLE-SHARP)
  (5 . ,DOUBLE-SHARP) (2 . ,DOUBLE-SHARP) (6 . ,DOUBLE-SHARP)
)

persianAltKeyAlterationOrder = #`(
  (6 . ,FLAT) (6 . ,KORON) (6 . ,DOUBLE-FLAT)
  (2 . ,FLAT) (2 . ,KORON) (2 . ,DOUBLE-FLAT)
  (5 . ,FLAT) (5 . ,KORON) (5 . ,DOUBLE-FLAT)
  (1 . ,FLAT) (1 . ,KORON) (1 . ,DOUBLE-FLAT)
  (4 . ,FLAT) (4 . ,KORON) (4 . ,DOUBLE-FLAT)
  (0 . ,FLAT) (0 . ,KORON) (0 . ,DOUBLE-FLAT)
  (3 . ,FLAT) (3 . ,KORON) (3 . ,DOUBLE-FLAT)

  (3 . ,SHARP) (3 . ,SORI) (3 . ,DOUBLE-SHARP)
  (0 . ,SHARP) (0 . ,SORI) (0 . ,DOUBLE-SHARP)
  (4 . ,SHARP) (4 . ,SORI) (4 . ,DOUBLE-SHARP)
  (1 . ,SHARP) (1 . ,SORI) (1 . ,DOUBLE-SHARP)
  (5 . ,SHARP) (5 . ,SORI) (5 . ,DOUBLE-SHARP)
  (2 . ,SHARP) (2 . ,SORI) (2 . ,DOUBLE-SHARP)
  (6 . ,SHARP) (6 . ,SORI) (6 . ,DOUBLE-SHARP)
)

% Register default key alteration order.

\layout {
  \context {
    \Score
    keyAlterationOrder = \persianKeyAlterationOrder
  }
}


% Make the inter-glyph spacing of Persian key signatures a bit wider
% than for Western music to overcome some inflexibility in LilyPond's
% handling of accidental elements: It only takes the relative vertical
% positions of natural accidentals into account for some additional
% padding; currently, this is hard-coded and can't be extended to
% handle other accidental glyphs.

persianKeySignaturePaddingPairs = #'(
  (("accidentals.flat" . "accidentals.flat") . 0.2)
  (("accidentals.flat" . "accidentals.flat.koron") . 0.2)
  (("accidentals.flat" . "accidentals.sharp") . 0.2)
  (("accidentals.flat" . "accidentals.sharp.sori") . 0.2)

  (("accidentals.flat.koron" . "accidentals.flat") . 0.2)
  (("accidentals.flat.koron" . "accidentals.flat.koron") . 0.2)
  (("accidentals.flat.koron" . "accidentals.sharp") . 0.2)
  (("accidentals.flat.koron" . "accidentals.sharp.sori") . 0.2)

  (("accidentals.sharp" . "accidentals.flat") . 0.2)
  (("accidentals.sharp" . "accidentals.flat.koron") . 0.2)
  (("accidentals.sharp" . "accidentals.sharp") . 0.2)
  (("accidentals.sharp" . "accidentals.sharp.sori") . 0.2)

  (("accidentals.sharp.sori" . "accidentals.flat") . 0.2)
  (("accidentals.sharp.sori" . "accidentals.flat.koron") . 0.2)
  (("accidentals.sharp.sori" . "accidentals.sharp") . 0.2)
  (("accidentals.sharp.sori" . "accidentals.sharp.sori") . 0.2)
)


% Register key signature inter-glyph spacing.

\layout {
  \context {
    \Score
    \override KeySignature.padding-pairs =
      \persianKeySignaturePaddingPairs
  }
}
