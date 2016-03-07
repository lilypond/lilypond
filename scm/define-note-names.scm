;;;; Common note names in various languages.
;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2010--2015 Valentin Villenave <valentin@villenave.net> et al:
;;;;
;;;; Copyright (C) 1996--2015 Han-Wen Nienhuys <hanwen@xs4all.nl> (Nederlands)
;;;; Copyright (C) 1998--2015 Jaume Obrador <jobrador@ipc4.uib.es> (Catalan)
;;;; Copyright (C) 1997--2015 Roland Meier <meier@informatik.th-darmstadt.de>
;;;;                Bjoern Jacke <bjoern.jacke@gmx.de> (Deutsch)
;;;; Copyright (C) 1996--2015 Han-Wen Nienhuys <hanwen@xs4all.nl> (English)
;;;; Copyright (C) 2002--2015 Carlos García Suárez <cgscqmp@terra.es>
;;;;                Maximiliano G. G. <mxgdvg@yahoo.it> (Espanol)
;;;; Copyright (C) 1998--2015 Paolo Zuliani <zuliap@easynet.it>
;;;;                Eric Wurbel <wurbel@univ-tln.fr> (Italiano)
;;;; Copyright (C) 1998--2015 Arvid Grøtting <arvidg@ifi.uio.no> (Norsk)
;;;; Copyright (C) 2004--2015 Pedro Kröger <kroeger@pedrokroeger.net> (Portugues)
;;;; Copyright (C) 2001--2015 Heikki Junes <heikki.junes@hut.fi> (Suomi)
;;;; Copyright (C) 1997--2015 Mats Bengtsson <mabe@violin.s3.kth.se> (Svenska)
;;;; Copyright (C) 2004--2015 Hendrik Maryns <hendrik.maryns@ugent.be> (Vlaams)
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


;;; Variable declaration
(define-public pitchnames '())
(define-public default-language "")
(define-public previous-pitchnames #f)

(define-public language-pitch-names
  `(
    ;; Language: Nederlands --------------------------------------------;
    ;;  Dutch note names -- LilyPond's default language.
    ;;  Han-Wen Nienhuys <hanwen@xs4all.nl>
    ;;
    ;;  es   = flat
    ;;  eses = double-flat
    ;;  eh   = quarter-tone flat
    ;;  eseh = three-quarter-tones flat
    ;;
    ;;  is   = sharp
    ;;  isis = double-sharp
    ;;  ih   = quarter-tone sharp
    ;;  isih = three-quarter-tones sharp
    ;;
    ;;  English: c  d  e  f  g  a  bf b
    ;;    Dutch: c  d  e  f  g  a  b  h

    (nederlands . (
                   (ceses . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
                   (ceseh . ,(ly:make-pitch -1 0 THREE-Q-FLAT))
                   (ces . ,(ly:make-pitch -1 0 FLAT))
                   (ceh . ,(ly:make-pitch -1 0 SEMI-FLAT))
                   (c . ,(ly:make-pitch -1 0 NATURAL))
                   (cih . ,(ly:make-pitch -1 0 SEMI-SHARP))
                   (cis . ,(ly:make-pitch -1 0 SHARP))
                   (cisih . ,(ly:make-pitch -1 0 THREE-Q-SHARP))
                   (cisis . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

                   (deses . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
                   (deseh . ,(ly:make-pitch -1 1 THREE-Q-FLAT))
                   (des . ,(ly:make-pitch -1 1 FLAT))
                   (deh . ,(ly:make-pitch -1 1 SEMI-FLAT))
                   (d . ,(ly:make-pitch -1 1 NATURAL))
                   (dih . ,(ly:make-pitch -1 1 SEMI-SHARP))
                   (dis . ,(ly:make-pitch -1 1 SHARP))
                   (disih . ,(ly:make-pitch -1 1 THREE-Q-SHARP))
                   (disis . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

                   (eeses . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
                   (eses . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
                   (eeseh . ,(ly:make-pitch -1 2 THREE-Q-FLAT))
                   (ees . ,(ly:make-pitch -1 2 FLAT))
                   (es . ,(ly:make-pitch -1 2 FLAT))
                   (eeh . ,(ly:make-pitch -1 2 SEMI-FLAT))
                   (e . ,(ly:make-pitch -1 2 NATURAL))
                   (eih . ,(ly:make-pitch -1 2 SEMI-SHARP))
                   (eis . ,(ly:make-pitch -1 2 SHARP))
                   (eisih . ,(ly:make-pitch -1 2 THREE-Q-SHARP))
                   (eisis . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

                   (feses . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
                   (feseh . ,(ly:make-pitch -1 3 THREE-Q-FLAT))
                   (fes . ,(ly:make-pitch -1 3 FLAT))
                   (feh . ,(ly:make-pitch -1 3 SEMI-FLAT))
                   (f . ,(ly:make-pitch -1 3 NATURAL))
                   (fih . ,(ly:make-pitch -1 3 SEMI-SHARP))
                   (fis . ,(ly:make-pitch -1 3 SHARP))
                   (fisih . ,(ly:make-pitch -1 3 THREE-Q-SHARP))
                   (fisis . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

                   (geses . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
                   (geseh . ,(ly:make-pitch -1 4 THREE-Q-FLAT))
                   (ges . ,(ly:make-pitch -1 4 FLAT))
                   (geh . ,(ly:make-pitch -1 4 SEMI-FLAT))
                   (g . ,(ly:make-pitch -1 4 NATURAL))
                   (gih . ,(ly:make-pitch -1 4 SEMI-SHARP))
                   (gis . ,(ly:make-pitch -1 4 SHARP))
                   (gisih . ,(ly:make-pitch -1 4 THREE-Q-SHARP))
                   (gisis . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

                   (aeses . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
                   (ases . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
                   (aeseh . ,(ly:make-pitch -1 5 THREE-Q-FLAT))
                   (aes . ,(ly:make-pitch -1 5 FLAT))
                   (as . ,(ly:make-pitch -1 5 FLAT))
                   (aeh . ,(ly:make-pitch -1 5 SEMI-FLAT))
                   (a . ,(ly:make-pitch -1 5 NATURAL))
                   (aih . ,(ly:make-pitch -1 5 SEMI-SHARP))
                   (ais . ,(ly:make-pitch -1 5 SHARP))
                   (aisih . ,(ly:make-pitch -1 5 THREE-Q-SHARP))
                   (aisis . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

                   (beses . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
                   (beseh . ,(ly:make-pitch -1 6 THREE-Q-FLAT))
                   (bes . ,(ly:make-pitch -1 6 FLAT))
                   (beh . ,(ly:make-pitch -1 6 SEMI-FLAT))
                   (b . ,(ly:make-pitch -1 6 NATURAL))
                   (bih . ,(ly:make-pitch -1 6 SEMI-SHARP))
                   (bis . ,(ly:make-pitch -1 6 SHARP))
                   (bisih . ,(ly:make-pitch -1 6 THREE-Q-SHARP))
                   (bisis . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
                   ))


    ;; Language: Catalan -----------------------------------------------;
    ;; Jaume Obrador <jobrador@ipc4.uib.es>
    ;;
    ;;  b  = flat (bemoll)
    ;;  bb = double-flat
    ;;
    ;;  d  = sharp (diesi)
    ;;  dd = double-sharp
    ;;
    ;;  s  = sharp (sostingut)
    ;;  ss = double-sharp
    ;;
    ;;  English: c   d   e   f   g   a   b
    ;;  Catalan: do  re  mi  fa  sol la  si

    (catalan . (
                (dobb . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
                (dob . ,(ly:make-pitch -1 0 FLAT))
                (do . ,(ly:make-pitch -1 0 NATURAL))
                (dod . ,(ly:make-pitch -1 0 SHARP))
                (dodd . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

                (rebb . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
                (reb . ,(ly:make-pitch -1 1 FLAT))
                (re . ,(ly:make-pitch -1 1 NATURAL))
                (red . ,(ly:make-pitch -1 1 SHARP))
                (redd . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

                (mibb . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
                (mib . ,(ly:make-pitch -1 2 FLAT))
                (mi . ,(ly:make-pitch -1 2 NATURAL))
                (mid . ,(ly:make-pitch -1 2 SHARP))
                (midd . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

                (fabb . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
                (fab . ,(ly:make-pitch -1 3 FLAT))
                (fa . ,(ly:make-pitch -1 3 NATURAL))
                (fad . ,(ly:make-pitch -1 3 SHARP))
                (fadd . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

                (solbb . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
                (solb . ,(ly:make-pitch -1 4 FLAT))
                (sol . ,(ly:make-pitch -1 4 NATURAL))
                (sold . ,(ly:make-pitch -1 4 SHARP))
                (soldd . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

                (labb . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
                (lab . ,(ly:make-pitch -1 5 FLAT))
                (la . ,(ly:make-pitch -1 5 NATURAL))
                (lad . ,(ly:make-pitch -1 5 SHARP))
                (ladd . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

                (sibb . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
                (sib . ,(ly:make-pitch -1 6 FLAT))
                (si . ,(ly:make-pitch -1 6 NATURAL))
                (sid . ,(ly:make-pitch -1 6 SHARP))
                (sidd . ,(ly:make-pitch -1 6 DOUBLE-SHARP))

                ;; Now that we have espanol.ly, should these be junked? --jcn
                (dos . ,(ly:make-pitch -1 0 SHARP))
                (doss . ,(ly:make-pitch -1 0 DOUBLE-SHARP))
                (res . ,(ly:make-pitch -1 1 SHARP))
                (ress . ,(ly:make-pitch -1 1 DOUBLE-SHARP))
                (mis . ,(ly:make-pitch -1 2 SHARP))
                (miss . ,(ly:make-pitch -1 2 DOUBLE-SHARP))
                (fas . ,(ly:make-pitch -1 3 SHARP))
                (fass . ,(ly:make-pitch -1 3 DOUBLE-SHARP))
                (sols . ,(ly:make-pitch -1 4 SHARP))
                (solss . ,(ly:make-pitch -1 4 DOUBLE-SHARP))
                (las . ,(ly:make-pitch -1 5 SHARP))
                (lass . ,(ly:make-pitch -1 5 DOUBLE-SHARP))
                (sis . ,(ly:make-pitch -1 6 SHARP))
                (siss . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
                ))


    ;; Language: Deutsch -----------------------------------------------;
    ;; Roland Meier <meier@informatik.th-darmstadt.de>,
    ;; Bjoern Jacke <bjoern.jacke@gmx.de>
    ;;
    ;;  es   = flat
    ;;  eses = double-flat
    ;;  eh   = quarter-tone flat
    ;;  eseh = three-quarter-tones flat
    ;;
    ;;  is   = sharp
    ;;  isis = double-sharp
    ;;  ih   = quarter-tone sharp
    ;;  isih = three-quarter-tones sharp
    ;;
    ;;  English: c  d  e  f  g  a  bf b
    ;;   German: c  d  e  f  g  a  b  h

    (deutsch . (
                (ceses . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
                (ceseh . ,(ly:make-pitch -1 0 THREE-Q-FLAT))
                (ces . ,(ly:make-pitch -1 0 FLAT))
                (ceh . ,(ly:make-pitch -1 0 SEMI-FLAT))
                (c . ,(ly:make-pitch -1 0 NATURAL))
                (cih . ,(ly:make-pitch -1 0 SEMI-SHARP))
                (cis . ,(ly:make-pitch -1 0 SHARP))
                (cisih . ,(ly:make-pitch -1 0 THREE-Q-SHARP))
                (cisis . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

                (deses . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
                (deseh . ,(ly:make-pitch -1 1 THREE-Q-FLAT))
                (des . ,(ly:make-pitch -1 1 FLAT))
                (deh . ,(ly:make-pitch -1 1 SEMI-FLAT))
                (d . ,(ly:make-pitch -1 1 NATURAL))
                (dih . ,(ly:make-pitch -1 1 SEMI-SHARP))
                (dis . ,(ly:make-pitch -1 1 SHARP))
                (disih . ,(ly:make-pitch -1 1 THREE-Q-SHARP))
                (disis . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

                (eses . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
                (eseh . ,(ly:make-pitch -1 2 THREE-Q-FLAT))
                (es . ,(ly:make-pitch -1 2 FLAT))
                (eeh . ,(ly:make-pitch -1 2 SEMI-FLAT))
                (e . ,(ly:make-pitch -1 2 NATURAL))
                (eih . ,(ly:make-pitch -1 2 SEMI-SHARP))
                (eis . ,(ly:make-pitch -1 2 SHARP))
                (eisih . ,(ly:make-pitch -1 2 THREE-Q-SHARP))
                (eisis . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

                (feses . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
                (feseh . ,(ly:make-pitch -1 3 THREE-Q-FLAT))
                (fes . ,(ly:make-pitch -1 3 FLAT))
                (feh . ,(ly:make-pitch -1 3 SEMI-FLAT))
                (f . ,(ly:make-pitch -1 3 NATURAL))
                (fih . ,(ly:make-pitch -1 3 SEMI-SHARP))
                (fis . ,(ly:make-pitch -1 3 SHARP))
                (fisih . ,(ly:make-pitch -1 3 THREE-Q-SHARP))
                (fisis . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

                (geses . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
                (geseh . ,(ly:make-pitch -1 4 THREE-Q-FLAT))
                (ges . ,(ly:make-pitch -1 4 FLAT))
                (geh . ,(ly:make-pitch -1 4 SEMI-FLAT))
                (g . ,(ly:make-pitch -1 4 NATURAL))
                (gih . ,(ly:make-pitch -1 4 SEMI-SHARP))
                (gis . ,(ly:make-pitch -1 4 SHARP))
                (gisih . ,(ly:make-pitch -1 4 THREE-Q-SHARP))
                (gisis . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

                (asas . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
                (ases . ,(ly:make-pitch -1 5 DOUBLE-FLAT))   ;; non-standard name for asas
                (asah . ,(ly:make-pitch -1 5 THREE-Q-FLAT))
                (aseh . ,(ly:make-pitch -1 5 THREE-Q-FLAT))  ;; non-standard name for asah
                (as . ,(ly:make-pitch -1 5 FLAT))
                (aeh . ,(ly:make-pitch -1 5 SEMI-FLAT))
                (a . ,(ly:make-pitch -1 5 NATURAL))
                (aih . ,(ly:make-pitch -1 5 SEMI-SHARP))
                (ais . ,(ly:make-pitch -1 5 SHARP))
                (aisih . ,(ly:make-pitch -1 5 THREE-Q-SHARP))
                (aisis . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

                (heses . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
                (heseh . ,(ly:make-pitch -1 6 THREE-Q-FLAT))
                (b . ,(ly:make-pitch -1 6 FLAT))
                (beh . ,(ly:make-pitch -1 6 SEMI-FLAT))
                (h . ,(ly:make-pitch -1 6 NATURAL))
                (hih . ,(ly:make-pitch -1 6 SEMI-SHARP))
                (his . ,(ly:make-pitch -1 6 SHARP))
                (hisih . ,(ly:make-pitch -1 6 THREE-Q-SHARP))
                (hisis . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
                ))


    ;; Language: English -----------------------------------------------;
    ;; Han-Wen Nienhuys <hanwen@xs4all.nl>
    ;;
    ;;  f   = flat
    ;;  ff  = double-flat
    ;;  qf  = quarter[-tone] flat
    ;;  tqf = three-quarter[-tones] flat
    ;;
    ;;  s   = sharp
    ;;  x   = double-sharp
    ;;  ss  = double-sharp
    ;;  qs  = quarter[-tone] sharp
    ;;  tqs = three-quarter[-tones] sharp

    (english . (
                (cff . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
                (ctqf . ,(ly:make-pitch -1 0 THREE-Q-FLAT))
                (cf . ,(ly:make-pitch -1 0 FLAT))
                (cqf . ,(ly:make-pitch -1 0 SEMI-FLAT))
                (c . ,(ly:make-pitch -1 0 NATURAL))
                (cqs . ,(ly:make-pitch -1 0 SEMI-SHARP))
                (cs . ,(ly:make-pitch -1 0 SHARP))
                (ctqs . ,(ly:make-pitch -1 0 THREE-Q-SHARP))
                (css . ,(ly:make-pitch -1 0 DOUBLE-SHARP))
                (cx . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

                (dff . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
                (dtqf . ,(ly:make-pitch -1 1 THREE-Q-FLAT))
                (df . ,(ly:make-pitch -1 1 FLAT))
                (dqf . ,(ly:make-pitch -1 1 SEMI-FLAT))
                (d . ,(ly:make-pitch -1 1 NATURAL))
                (dqs . ,(ly:make-pitch -1 1 SEMI-SHARP))
                (ds . ,(ly:make-pitch -1 1 SHARP))
                (dtqs . ,(ly:make-pitch -1 1 THREE-Q-SHARP))
                (dss . ,(ly:make-pitch -1 1 DOUBLE-SHARP))
                (dx . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

                (eff . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
                (etqf . ,(ly:make-pitch -1 2 THREE-Q-FLAT))
                (ef . ,(ly:make-pitch -1 2 FLAT))
                (eqf . ,(ly:make-pitch -1 2 SEMI-FLAT))
                (e . ,(ly:make-pitch -1 2 NATURAL))
                (eqs . ,(ly:make-pitch -1 2 SEMI-SHARP))
                (es . ,(ly:make-pitch -1 2 SHARP))
                (etqs . ,(ly:make-pitch -1 2 THREE-Q-SHARP))
                (ess . ,(ly:make-pitch -1 2 DOUBLE-SHARP))
                (ex . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

                (fff . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
                (ftqf . ,(ly:make-pitch -1 3 THREE-Q-FLAT))
                (ff . ,(ly:make-pitch -1 3 FLAT))
                (fqf . ,(ly:make-pitch -1 3 SEMI-FLAT))
                (f . ,(ly:make-pitch -1 3 NATURAL))
                (fqs . ,(ly:make-pitch -1 3 SEMI-SHARP))
                (fs . ,(ly:make-pitch -1 3 SHARP))
                (ftqs . ,(ly:make-pitch -1 3 THREE-Q-SHARP))
                (fss . ,(ly:make-pitch -1 3 DOUBLE-SHARP))
                (fx . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

                (gff . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
                (gtqf . ,(ly:make-pitch -1 4 THREE-Q-FLAT))
                (gf . ,(ly:make-pitch -1 4 FLAT))
                (gqf . ,(ly:make-pitch -1 4 SEMI-FLAT))
                (g . ,(ly:make-pitch -1 4 NATURAL))
                (gqs . ,(ly:make-pitch -1 4 SEMI-SHARP))
                (gs . ,(ly:make-pitch -1 4 SHARP))
                (gtqs . ,(ly:make-pitch -1 4 THREE-Q-SHARP))
                (gss . ,(ly:make-pitch -1 4 DOUBLE-SHARP))
                (gx . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

                (aff . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
                (atqf . ,(ly:make-pitch -1 5 THREE-Q-FLAT))
                (af . ,(ly:make-pitch -1 5 FLAT))
                (aqf . ,(ly:make-pitch -1 5 SEMI-FLAT))
                (a . ,(ly:make-pitch -1 5 NATURAL))
                (aqs . ,(ly:make-pitch -1 5 SEMI-SHARP))
                (as . ,(ly:make-pitch -1 5 SHARP))
                (atqs . ,(ly:make-pitch -1 5 THREE-Q-SHARP))
                (ass . ,(ly:make-pitch -1 5 DOUBLE-SHARP))
                (ax . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

                (bff . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
                (btqf . ,(ly:make-pitch -1 6 THREE-Q-FLAT))
                (bf . ,(ly:make-pitch -1 6 FLAT))
                (bqf . ,(ly:make-pitch -1 6 SEMI-FLAT))
                (b . ,(ly:make-pitch -1 6 NATURAL))
                (bqs . ,(ly:make-pitch -1 6 SEMI-SHARP))
                (bs . ,(ly:make-pitch -1 6 SHARP))
                (btqs . ,(ly:make-pitch -1 6 THREE-Q-SHARP))
                (bss . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
                (bx . ,(ly:make-pitch -1 6 DOUBLE-SHARP))

                (c-flatflat . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
                (c-flat . ,(ly:make-pitch -1 0 FLAT))
                (c-natural . ,(ly:make-pitch -1 0 NATURAL))
                (c-sharp . ,(ly:make-pitch -1 0 SHARP))
                (c-sharpsharp . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

                (d-flatflat . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
                (d-flat . ,(ly:make-pitch -1 1 FLAT))
                (d-natural . ,(ly:make-pitch -1 1 NATURAL))
                (d-sharp . ,(ly:make-pitch -1 1 SHARP))
                (d-sharpsharp . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

                (e-flatflat . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
                (e-flat . ,(ly:make-pitch -1 2 FLAT))
                (e-natural . ,(ly:make-pitch -1 2 NATURAL))
                (e-sharp . ,(ly:make-pitch -1 2 SHARP))
                (e-sharpsharp . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

                (f-flatflat . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
                (f-flat . ,(ly:make-pitch -1 3 FLAT))
                (f-natural . ,(ly:make-pitch -1 3 NATURAL))
                (f-sharp . ,(ly:make-pitch -1 3 SHARP))
                (f-sharpsharp . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

                (g-flatflat . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
                (g-flat . ,(ly:make-pitch -1 4 FLAT))
                (g-natural . ,(ly:make-pitch -1 4 NATURAL))
                (g-sharp . ,(ly:make-pitch -1 4 SHARP))
                (g-sharpsharp . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

                (a-flatflat . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
                (a-flat . ,(ly:make-pitch -1 5 FLAT))
                (a-natural . ,(ly:make-pitch -1 5 NATURAL))
                (a-sharp . ,(ly:make-pitch -1 5 SHARP))
                (a-sharpsharp . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

                (b-flatflat . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
                (b-flat . ,(ly:make-pitch -1 6 FLAT))
                (b-natural . ,(ly:make-pitch -1 6 NATURAL))
                (b-sharp . ,(ly:make-pitch -1 6 SHARP))
                (b-sharpsharp . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
                ))


    ;; Language: Espanol -----------------------------------------------;
    ;; Carlos García Suárez <cgscqmp@terra.es>,
    ;; Maximiliano G. G. <mxgdvg@yahoo.it>
    ;;
    ;;  b   = flat (bemol)
    ;;  bb  = double-flat
    ;;  cb  = quarter-tone flat (cuarto [de tono de] bemol)
    ;;  tcb = three-quarter-tones flat (tres cuartos [de tonos de] bemol)
    ;;
    ;;  s   = sharp (sostenido)
    ;;  x   = double-sharp
    ;;  ss  = double-sharp
    ;;  cs  = quarter-tone sharp (cuarto [de tono de] sostenido)
    ;;  tcs = three-quarter-tones sharp (tres cuartos [de tonos de] sostenido)
    ;;
    ;;  English: c   d   e   f   g   a   b
    ;;  Spanish: do  re  mi  fa  sol la  si

    (espanol . (
                (dobb . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
                (dotcb . ,(ly:make-pitch -1 0 THREE-Q-FLAT))
                (dob . ,(ly:make-pitch -1 0 FLAT))
                (docb . ,(ly:make-pitch -1 0 SEMI-FLAT))
                (do . ,(ly:make-pitch -1 0 NATURAL))
                (docs . ,(ly:make-pitch -1 0 SEMI-SHARP))
                (dos . ,(ly:make-pitch -1 0 SHARP))
                (dotcs . ,(ly:make-pitch -1 0 THREE-Q-SHARP))
                (doss . ,(ly:make-pitch -1 0 DOUBLE-SHARP))
                (dox . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

                (rebb . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
                (retcb . ,(ly:make-pitch -1 1 THREE-Q-FLAT))
                (reb . ,(ly:make-pitch -1 1 FLAT))
                (recb . ,(ly:make-pitch -1 1 SEMI-FLAT))
                (re . ,(ly:make-pitch -1 1 NATURAL))
                (recs . ,(ly:make-pitch -1 1 SEMI-SHARP))
                (res . ,(ly:make-pitch -1 1 SHARP))
                (retcs . ,(ly:make-pitch -1 1 THREE-Q-SHARP))
                (ress . ,(ly:make-pitch -1 1 DOUBLE-SHARP))
                (rex . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

                (mibb . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
                (mitcb . ,(ly:make-pitch -1 2 THREE-Q-FLAT))
                (mib . ,(ly:make-pitch -1 2 FLAT))
                (micb . ,(ly:make-pitch -1 2 SEMI-FLAT))
                (mi . ,(ly:make-pitch -1 2 NATURAL))
                (mics . ,(ly:make-pitch -1 2 SEMI-SHARP))
                (mis . ,(ly:make-pitch -1 2 SHARP))
                (mitcs . ,(ly:make-pitch -1 2 THREE-Q-SHARP))
                (miss . ,(ly:make-pitch -1 2 DOUBLE-SHARP))
                (mix . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

                (fabb . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
                (fatcb . ,(ly:make-pitch -1 3 THREE-Q-FLAT))
                (fab . ,(ly:make-pitch -1 3 FLAT))
                (facb . ,(ly:make-pitch -1 3 SEMI-FLAT))
                (fa . ,(ly:make-pitch -1 3 NATURAL))
                (facs . ,(ly:make-pitch -1 3 SEMI-SHARP))
                (fas . ,(ly:make-pitch -1 3 SHARP))
                (fatcs . ,(ly:make-pitch -1 3 THREE-Q-SHARP))
                (fass . ,(ly:make-pitch -1 3 DOUBLE-SHARP))
                (fax . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

                (solbb . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
                (soltcb . ,(ly:make-pitch -1 4 THREE-Q-FLAT))
                (solb . ,(ly:make-pitch -1 4 FLAT))
                (solcb . ,(ly:make-pitch -1 4 SEMI-FLAT))
                (sol . ,(ly:make-pitch -1 4 NATURAL))
                (solcs . ,(ly:make-pitch -1 4 SEMI-SHARP))
                (sols . ,(ly:make-pitch -1 4 SHARP))
                (soltcs . ,(ly:make-pitch -1 4 THREE-Q-SHARP))
                (solss . ,(ly:make-pitch -1 4 DOUBLE-SHARP))
                (solx . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

                (labb . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
                (latcb . ,(ly:make-pitch -1 5 THREE-Q-FLAT))
                (lab . ,(ly:make-pitch -1 5 FLAT))
                (lacb . ,(ly:make-pitch -1 5 SEMI-FLAT))
                (la . ,(ly:make-pitch -1 5 NATURAL))
                (lacs . ,(ly:make-pitch -1 5 SEMI-SHARP))
                (las . ,(ly:make-pitch -1 5 SHARP))
                (latcs . ,(ly:make-pitch -1 5 THREE-Q-SHARP))
                (lass . ,(ly:make-pitch -1 5 DOUBLE-SHARP))
                (lax . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

                (sibb . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
                (sitcb . ,(ly:make-pitch -1 6 THREE-Q-FLAT))
                (sib . ,(ly:make-pitch -1 6 FLAT))
                (sicb . ,(ly:make-pitch -1 6 SEMI-FLAT))
                (si . ,(ly:make-pitch -1 6 NATURAL))
                (sics . ,(ly:make-pitch -1 6 SEMI-SHARP))
                (sis . ,(ly:make-pitch -1 6 SHARP))
                (sitcs . ,(ly:make-pitch -1 6 THREE-Q-SHARP))
                (siss . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
                (six . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
                ))


    ;; Language: Français ----------------------------------------------;
    ;; Valentin Villenave <valentin@villenave.net>,
    ;;
    ;;  b   = flat (bémol)
    ;;  bb  = double-flat
    ;;  sb  = quarter-tone flat (demi-bémol)
    ;;  bsb = three-quarter-tones flat
    ;;
    ;;  d   = sharp (dièse)
    ;;  dd  = double-sharp
    ;;  x   = double-sharp
    ;;  sd  = quarter-tone sharp (demi-dièse)
    ;;  dsd = three-quarter-tones sharp
    ;;
    ;;  English: c   d   e   f   g   a   b
    ;;  French:  do  ré  mi  fa  sol la  si

    (français . (
                 (dobb . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
                 (dobsb . ,(ly:make-pitch -1 0 THREE-Q-FLAT))
                 (dob . ,(ly:make-pitch -1 0 FLAT))
                 (dosb . ,(ly:make-pitch -1 0 SEMI-FLAT))
                 (do . ,(ly:make-pitch -1 0 NATURAL))
                 (dosd . ,(ly:make-pitch -1 0 SEMI-SHARP))
                 (dod . ,(ly:make-pitch -1 0 SHARP))
                 (dodsd . ,(ly:make-pitch -1 0 THREE-Q-SHARP))
                 (dodd . ,(ly:make-pitch -1 0 DOUBLE-SHARP))
                 (dox . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

                 (rébb . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
                 (rébsb . ,(ly:make-pitch -1 1 THREE-Q-FLAT))
                 (réb . ,(ly:make-pitch -1 1 FLAT))
                 (résb . ,(ly:make-pitch -1 1 SEMI-FLAT))
                 (ré . ,(ly:make-pitch -1 1 NATURAL))
                 (résd . ,(ly:make-pitch -1 1 SEMI-SHARP))
                 (réd . ,(ly:make-pitch -1 1 SHARP))
                 (rédsd . ,(ly:make-pitch -1 1 THREE-Q-SHARP))
                 (rédd . ,(ly:make-pitch -1 1 DOUBLE-SHARP))
                 (réx . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

                 (rebb . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
                 (rebsb . ,(ly:make-pitch -1 1 THREE-Q-FLAT))
                 (reb . ,(ly:make-pitch -1 1 FLAT))
                 (resb . ,(ly:make-pitch -1 1 SEMI-FLAT))
                 (re . ,(ly:make-pitch -1 1 NATURAL))
                 (resd . ,(ly:make-pitch -1 1 SEMI-SHARP))
                 (red . ,(ly:make-pitch -1 1 SHARP))
                 (redsd . ,(ly:make-pitch -1 1 THREE-Q-SHARP))
                 (redd . ,(ly:make-pitch -1 1 DOUBLE-SHARP))
                 (rex . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

                 (mibb . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
                 (mibsb . ,(ly:make-pitch -1 2 THREE-Q-FLAT))
                 (mib . ,(ly:make-pitch -1 2 FLAT))
                 (misb . ,(ly:make-pitch -1 2 SEMI-FLAT))
                 (mi . ,(ly:make-pitch -1 2 NATURAL))
                 (misd . ,(ly:make-pitch -1 2 SEMI-SHARP))
                 (mid . ,(ly:make-pitch -1 2 SHARP))
                 (midsd . ,(ly:make-pitch -1 2 THREE-Q-SHARP))
                 (midd . ,(ly:make-pitch -1 2 DOUBLE-SHARP))
                 (mix . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

                 (fabb . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
                 (fabsb . ,(ly:make-pitch -1 3 THREE-Q-FLAT))
                 (fab . ,(ly:make-pitch -1 3 FLAT))
                 (fasb . ,(ly:make-pitch -1 3 SEMI-FLAT))
                 (fa . ,(ly:make-pitch -1 3 NATURAL))
                 (fasd . ,(ly:make-pitch -1 3 SEMI-SHARP))
                 (fad . ,(ly:make-pitch -1 3 SHARP))
                 (fadsd . ,(ly:make-pitch -1 3 THREE-Q-SHARP))
                 (fadd . ,(ly:make-pitch -1 3 DOUBLE-SHARP))
                 (fax . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

                 (solbb . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
                 (solbsb . ,(ly:make-pitch -1 4 THREE-Q-FLAT))
                 (solb . ,(ly:make-pitch -1 4 FLAT))
                 (solsb . ,(ly:make-pitch -1 4 SEMI-FLAT))
                 (sol . ,(ly:make-pitch -1 4 NATURAL))
                 (solsd . ,(ly:make-pitch -1 4 SEMI-SHARP))
                 (sold . ,(ly:make-pitch -1 4 SHARP))
                 (soldsd . ,(ly:make-pitch -1 4 THREE-Q-SHARP))
                 (soldd . ,(ly:make-pitch -1 4 DOUBLE-SHARP))
                 (solx . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

                 (labb . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
                 (labsb . ,(ly:make-pitch -1 5 THREE-Q-FLAT))
                 (lab . ,(ly:make-pitch -1 5 FLAT))
                 (lasb . ,(ly:make-pitch -1 5 SEMI-FLAT))
                 (la . ,(ly:make-pitch -1 5 NATURAL))
                 (lasd . ,(ly:make-pitch -1 5 SEMI-SHARP))
                 (lad . ,(ly:make-pitch -1 5 SHARP))
                 (ladsd . ,(ly:make-pitch -1 5 THREE-Q-SHARP))
                 (ladd . ,(ly:make-pitch -1 5 DOUBLE-SHARP))
                 (lax . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

                 (sibb . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
                 (sibsb . ,(ly:make-pitch -1 6 THREE-Q-FLAT))
                 (sib . ,(ly:make-pitch -1 6 FLAT))
                 (sisb . ,(ly:make-pitch -1 6 SEMI-FLAT))
                 (si . ,(ly:make-pitch -1 6 NATURAL))
                 (sisd . ,(ly:make-pitch -1 6 SEMI-SHARP))
                 (sid . ,(ly:make-pitch -1 6 SHARP))
                 (sidsd . ,(ly:make-pitch -1 6 THREE-Q-SHARP))
                 (sidd . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
                 (six . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
                 ))


    ;; Language: Italiano ----------------------------------------------;
    ;; Paolo Zuliani <zuliap@easynet.it>,
    ;; Eric Wurbel <wurbel@univ-tln.fr>
    ;;
    ;;  b   = flat (bemolle)
    ;;  bb  = double-flat
    ;;  sb  = quarter-tone flat (semi-bemolle)
    ;;  bsb = three-quarter-tones flat
    ;;
    ;;  d   = sharp (diesis)
    ;;  dd  = double-sharp
    ;;  sd  = quarter-tone sharp (semi-diesis)
    ;;  dsd = three-quarter-tones sharp
    ;;
    ;;  English: c   d   e   f   g   a   b
    ;;  Italian: do  re  mi  fa  sol la  si

    (italiano . (
                 (dobb . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
                 (dobsb . ,(ly:make-pitch -1 0 THREE-Q-FLAT))
                 (dob . ,(ly:make-pitch -1 0 FLAT))
                 (dosb . ,(ly:make-pitch -1 0 SEMI-FLAT))
                 (do . ,(ly:make-pitch -1 0 NATURAL))
                 (dosd . ,(ly:make-pitch -1 0 SEMI-SHARP))
                 (dod . ,(ly:make-pitch -1 0 SHARP))
                 (dodsd . ,(ly:make-pitch -1 0 THREE-Q-SHARP))
                 (dodd . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

                 (rebb . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
                 (rebsb . ,(ly:make-pitch -1 1 THREE-Q-FLAT))
                 (reb . ,(ly:make-pitch -1 1 FLAT))
                 (resb . ,(ly:make-pitch -1 1 SEMI-FLAT))
                 (re . ,(ly:make-pitch -1 1 NATURAL))
                 (resd . ,(ly:make-pitch -1 1 SEMI-SHARP))
                 (red . ,(ly:make-pitch -1 1 SHARP))
                 (redsd . ,(ly:make-pitch -1 1 THREE-Q-SHARP))
                 (redd . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

                 (mibb . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
                 (mibsb . ,(ly:make-pitch -1 2 THREE-Q-FLAT))
                 (mib . ,(ly:make-pitch -1 2 FLAT))
                 (misb . ,(ly:make-pitch -1 2 SEMI-FLAT))
                 (mi . ,(ly:make-pitch -1 2 NATURAL))
                 (misd . ,(ly:make-pitch -1 2 SEMI-SHARP))
                 (mid . ,(ly:make-pitch -1 2 SHARP))
                 (midsd . ,(ly:make-pitch -1 2 THREE-Q-SHARP))
                 (midd . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

                 (fabb . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
                 (fabsb . ,(ly:make-pitch -1 3 THREE-Q-FLAT))
                 (fab . ,(ly:make-pitch -1 3 FLAT))
                 (fasb . ,(ly:make-pitch -1 3 SEMI-FLAT))
                 (fa . ,(ly:make-pitch -1 3 NATURAL))
                 (fasd . ,(ly:make-pitch -1 3 SEMI-SHARP))
                 (fad . ,(ly:make-pitch -1 3 SHARP))
                 (fadsd . ,(ly:make-pitch -1 3 THREE-Q-SHARP))
                 (fadd . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

                 (solbb . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
                 (solbsb . ,(ly:make-pitch -1 4 THREE-Q-FLAT))
                 (solb . ,(ly:make-pitch -1 4 FLAT))
                 (solsb . ,(ly:make-pitch -1 4 SEMI-FLAT))
                 (sol . ,(ly:make-pitch -1 4 NATURAL))
                 (solsd . ,(ly:make-pitch -1 4 SEMI-SHARP))
                 (sold . ,(ly:make-pitch -1 4 SHARP))
                 (soldsd . ,(ly:make-pitch -1 4 THREE-Q-SHARP))
                 (soldd . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

                 (labb . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
                 (labsb . ,(ly:make-pitch -1 5 THREE-Q-FLAT))
                 (lab . ,(ly:make-pitch -1 5 FLAT))
                 (lasb . ,(ly:make-pitch -1 5 SEMI-FLAT))
                 (la . ,(ly:make-pitch -1 5 NATURAL))
                 (lasd . ,(ly:make-pitch -1 5 SEMI-SHARP))
                 (lad . ,(ly:make-pitch -1 5 SHARP))
                 (ladsd . ,(ly:make-pitch -1 5 THREE-Q-SHARP))
                 (ladd . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

                 (sibb . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
                 (sibsb . ,(ly:make-pitch -1 6 THREE-Q-FLAT))
                 (sib . ,(ly:make-pitch -1 6 FLAT))
                 (sisb . ,(ly:make-pitch -1 6 SEMI-FLAT))
                 (si . ,(ly:make-pitch -1 6 NATURAL))
                 (sisd . ,(ly:make-pitch -1 6 SEMI-SHARP))
                 (sid . ,(ly:make-pitch -1 6 SHARP))
                 (sidsd . ,(ly:make-pitch -1 6 THREE-Q-SHARP))
                 (sidd . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
                 ))


    ;; Language: Norsk -------------------------------------------------;
    ;; Arvid Grøtting <arvidg@ifi.uio.no>
    ;;
    ;;  es     = flat
    ;;  ess    = flat
    ;;  eses   = double-flat
    ;;  essess = double-flat
    ;;
    ;;  is     = sharp
    ;;  iss    = sharp
    ;;  isis   = double-sharp
    ;;  ississ = double-sharp
    ;;
    ;;    English: c  d  e  f  g  a  bf b
    ;;  Norwegian: c  d  e  f  g  a  b  h

    (norsk . (
              (ceses . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
              (cessess . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
              (ces . ,(ly:make-pitch -1 0 FLAT))
              (cess . ,(ly:make-pitch -1 0 FLAT))
              (c . ,(ly:make-pitch -1 0 NATURAL))
              (cis . ,(ly:make-pitch -1 0 SHARP))
              (ciss . ,(ly:make-pitch -1 0 SHARP))
              (cisis . ,(ly:make-pitch -1 0 DOUBLE-SHARP))
              (cississ . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

              (deses . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
              (dessess . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
              (des . ,(ly:make-pitch -1 1 FLAT))
              (dess . ,(ly:make-pitch -1 1 FLAT))
              (d . ,(ly:make-pitch -1 1 NATURAL))
              (dis . ,(ly:make-pitch -1 1 SHARP))
              (diss . ,(ly:make-pitch -1 1 SHARP))
              (disis . ,(ly:make-pitch -1 1 DOUBLE-SHARP))
              (dississ . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

              (eeses . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
              (eessess . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
              (eses . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
              (essess . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
              (ees . ,(ly:make-pitch -1 2 FLAT))
              (eess . ,(ly:make-pitch -1 2 FLAT))
              (es . ,(ly:make-pitch -1 2 FLAT))
              (ess . ,(ly:make-pitch -1 2 FLAT))
              (e . ,(ly:make-pitch -1 2 NATURAL))
              (eis . ,(ly:make-pitch -1 2 SHARP))
              (eiss . ,(ly:make-pitch -1 2 SHARP))
              (eisis . ,(ly:make-pitch -1 2 DOUBLE-SHARP))
              (eississ . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

              (feses . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
              (fessess . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
              (fes . ,(ly:make-pitch -1 3 FLAT))
              (fess . ,(ly:make-pitch -1 3 FLAT))
              (f . ,(ly:make-pitch -1 3 NATURAL))
              (fis . ,(ly:make-pitch -1 3 SHARP))
              (fiss . ,(ly:make-pitch -1 3 SHARP))
              (fisis . ,(ly:make-pitch -1 3 DOUBLE-SHARP))
              (fississ . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

              (geses . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
              (gessess . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
              (ges . ,(ly:make-pitch -1 4 FLAT))
              (gess . ,(ly:make-pitch -1 4 FLAT))
              (g . ,(ly:make-pitch -1 4 NATURAL))
              (g . ,(ly:make-pitch -1 4 NATURAL))
              (gis . ,(ly:make-pitch -1 4 SHARP))
              (giss . ,(ly:make-pitch -1 4 SHARP))
              (gisis . ,(ly:make-pitch -1 4 DOUBLE-SHARP))
              (gississ . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

              (aeses . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
              (aessess . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
              (ases . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
              (assess . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
              (aes . ,(ly:make-pitch -1 5 FLAT))
              (aess . ,(ly:make-pitch -1 5 FLAT))
              (as . ,(ly:make-pitch -1 5 FLAT))
              (ass . ,(ly:make-pitch -1 5 FLAT))
              (a . ,(ly:make-pitch -1 5 NATURAL))
              (ais . ,(ly:make-pitch -1 5 SHARP))
              (aiss . ,(ly:make-pitch -1 5 SHARP))
              (aisis . ,(ly:make-pitch -1 5 DOUBLE-SHARP))
              (aississ . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

              (bes . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
              (bess . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
              (b . ,(ly:make-pitch -1 6 FLAT))
              (b . ,(ly:make-pitch -1 6 FLAT))
              (h . ,(ly:make-pitch -1 6 NATURAL))
              (his . ,(ly:make-pitch -1 6 SHARP))
              (hiss . ,(ly:make-pitch -1 6 SHARP))
              (hisis . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
              (hississ . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
              ))


    ;; Language: Portugues ---------------------------------------------;
    ;; Pedro Kröger <kroeger@pedrokroeger.net>
    ;;
    ;;  b    = flat (bemol)
    ;;  bb   = double-flat
    ;;  bqt  = quarter-tone flat
    ;;  btqt = three-quarter-tones flat
    ;;
    ;;  s    = sharp (sustenido)
    ;;  ss   = double-sharp
    ;;  sqt  = quarter-tone sharp
    ;;  stqt = three-quarter-tones sharp
    ;;
    ;;     English: c   d   e   f   g   a   b
    ;;  Portuguese: do  re  mi  fa  sol la  si

    (portugues . (
                  (dobb . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
                  (dobtqt . ,(ly:make-pitch -1 0 THREE-Q-FLAT))
                  (dob . ,(ly:make-pitch -1 0 FLAT))
                  (dobqt . ,(ly:make-pitch -1 0 SEMI-FLAT))
                  (do . ,(ly:make-pitch -1 0 NATURAL))
                  (dosqt . ,(ly:make-pitch -1 0 SEMI-SHARP))
                  (dos . ,(ly:make-pitch -1 0 SHARP))
                  (dostqt . ,(ly:make-pitch -1 0 THREE-Q-SHARP))
                  (doss . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

                  (rebb . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
                  (rebtqt . ,(ly:make-pitch -1 1 THREE-Q-FLAT))
                  (reb . ,(ly:make-pitch -1 1 FLAT))
                  (rebqt . ,(ly:make-pitch -1 1 SEMI-FLAT))
                  (re . ,(ly:make-pitch -1 1 NATURAL))
                  (resqt . ,(ly:make-pitch -1 1 SEMI-SHARP))
                  (res . ,(ly:make-pitch -1 1 SHARP))
                  (restqt . ,(ly:make-pitch -1 1 THREE-Q-SHARP))
                  (ress . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

                  (mibb . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
                  (mibtqt . ,(ly:make-pitch -1 2 THREE-Q-FLAT))
                  (mib . ,(ly:make-pitch -1 2 FLAT))
                  (mibqt . ,(ly:make-pitch -1 2 SEMI-FLAT))
                  (mi . ,(ly:make-pitch -1 2 NATURAL))
                  (misqt . ,(ly:make-pitch -1 2 SEMI-SHARP))
                  (mis . ,(ly:make-pitch -1 2 SHARP))
                  (mistqt . ,(ly:make-pitch -1 2 THREE-Q-SHARP))
                  (miss . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

                  (fabb . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
                  (fabtqt . ,(ly:make-pitch -1 3 THREE-Q-FLAT))
                  (fab . ,(ly:make-pitch -1 3 FLAT))
                  (fabqt . ,(ly:make-pitch -1 3 SEMI-FLAT))
                  (fa . ,(ly:make-pitch -1 3 NATURAL))
                  (fasqt . ,(ly:make-pitch -1 3 SEMI-SHARP))
                  (fas . ,(ly:make-pitch -1 3 SHARP))
                  (fastqt . ,(ly:make-pitch -1 3 THREE-Q-SHARP))
                  (fass . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

                  (solbb . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
                  (solbtqt . ,(ly:make-pitch -1 4 THREE-Q-FLAT))
                  (solb . ,(ly:make-pitch -1 4 FLAT))
                  (solbqt . ,(ly:make-pitch -1 4 SEMI-FLAT))
                  (sol . ,(ly:make-pitch -1 4 NATURAL))
                  (solsqt . ,(ly:make-pitch -1 4 SEMI-SHARP))
                  (sols . ,(ly:make-pitch -1 4 SHARP))
                  (solstqt . ,(ly:make-pitch -1 4 THREE-Q-SHARP))
                  (solss . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

                  (labb . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
                  (labtqt . ,(ly:make-pitch -1 5 THREE-Q-FLAT))
                  (lab . ,(ly:make-pitch -1 5 FLAT))
                  (labqt . ,(ly:make-pitch -1 5 SEMI-FLAT))
                  (la . ,(ly:make-pitch -1 5 NATURAL))
                  (lasqt . ,(ly:make-pitch -1 5 SEMI-SHARP))
                  (las . ,(ly:make-pitch -1 5 SHARP))
                  (lastqt . ,(ly:make-pitch -1 5 THREE-Q-SHARP))
                  (lass . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

                  (sibb . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
                  (sibtqt . ,(ly:make-pitch -1 6 THREE-Q-FLAT))
                  (sib . ,(ly:make-pitch -1 6 FLAT))
                  (sibqt . ,(ly:make-pitch -1 6 SEMI-FLAT))
                  (si . ,(ly:make-pitch -1 6 NATURAL))
                  (sisqt . ,(ly:make-pitch -1 6 SEMI-SHARP))
                  (sis . ,(ly:make-pitch -1 6 SHARP))
                  (sistqt . ,(ly:make-pitch -1 6 THREE-Q-SHARP))
                  (siss . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
                  ))


    ;; Language: Suomi -------------------------------------------------;
    ;; Heikki Junes <heikki.junes@hut.fi>
    ;;
    ;;  es   = flat
    ;;  eses = double-flat
    ;;
    ;;  is   = sharp
    ;;  isis = double-sharp
    ;;
    ;;  English: c  d  e  f  g  a  bf b
    ;;  Finnish: c  d  e  f  g  a  b  h

    (suomi . (
              (ceses . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
              (ces . ,(ly:make-pitch -1 0 FLAT))
              (c . ,(ly:make-pitch -1 0 NATURAL))
              (cis . ,(ly:make-pitch -1 0 SHARP))
              (cisis . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

              (deses . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
              (des . ,(ly:make-pitch -1 1 FLAT))
              (d . ,(ly:make-pitch -1 1 NATURAL))
              (dis . ,(ly:make-pitch -1 1 SHARP))
              (disis . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

              (eses . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
              (es . ,(ly:make-pitch -1 2 FLAT))
              (e . ,(ly:make-pitch -1 2 NATURAL))
              (eis . ,(ly:make-pitch -1 2 SHARP))
              (eisis . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

              (feses . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
              (fes . ,(ly:make-pitch -1 3 FLAT))
              (f . ,(ly:make-pitch -1 3 NATURAL))
              (fis . ,(ly:make-pitch -1 3 SHARP))
              (fisis . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

              (geses . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
              (ges . ,(ly:make-pitch -1 4 FLAT))
              (g . ,(ly:make-pitch -1 4 NATURAL))
              (gis . ,(ly:make-pitch -1 4 SHARP))
              (gisis . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

              (asas . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
              (ases . ,(ly:make-pitch -1 5 DOUBLE-FLAT))   ;; non-standard name for asas
              (as . ,(ly:make-pitch -1 5 FLAT))
              (a . ,(ly:make-pitch -1 5 NATURAL))
              (ais . ,(ly:make-pitch -1 5 SHARP))
              (aisis . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

              (bb . ,(ly:make-pitch -1 6 DOUBLE-FLAT)) ;; should be bes; kept for downwards compatibility
              (bes . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
              (heses . ,(ly:make-pitch -1 6 DOUBLE-FLAT))  ;; non-standard name for bb
              (b . ,(ly:make-pitch -1 6 FLAT))
              (h . ,(ly:make-pitch -1 6 NATURAL))
              (his . ,(ly:make-pitch -1 6 SHARP))
              (hisis . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
              ))


    ;; Language: Svenska -----------------------------------------------;
    ;; Mats Bengtsson <mabe@violin.s3.kth.se>
    ;;
    ;;  ess    = flat
    ;;  essess = double-flat
    ;;
    ;;  iss    = sharp
    ;;  ississ = double-sharp
    ;;
    ;;  English: c  d  e  f  g  a  bf b
    ;;  Swedish: c  d  e  f  g  a  b  h

    (svenska . (
                (cessess . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
                (cess . ,(ly:make-pitch -1 0 FLAT))
                (c . ,(ly:make-pitch -1 0 NATURAL))
                (ciss . ,(ly:make-pitch -1 0 SHARP))
                (cississ . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

                (dessess . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
                (dess . ,(ly:make-pitch -1 1 FLAT))
                (d . ,(ly:make-pitch -1 1 NATURAL))
                (diss . ,(ly:make-pitch -1 1 SHARP))
                (dississ . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

                (essess . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
                (ess . ,(ly:make-pitch -1 2 FLAT))
                (e . ,(ly:make-pitch -1 2 NATURAL))
                (eiss . ,(ly:make-pitch -1 2 SHARP))
                (eississ . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

                (fessess . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
                (fess . ,(ly:make-pitch -1 3 FLAT))
                (f . ,(ly:make-pitch -1 3 NATURAL))
                (fiss . ,(ly:make-pitch -1 3 SHARP))
                (fississ . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

                (gessess . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
                (gess . ,(ly:make-pitch -1 4 FLAT))
                (g . ,(ly:make-pitch -1 4 NATURAL))
                (giss . ,(ly:make-pitch -1 4 SHARP))
                (gississ . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

                (assess . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
                (ass . ,(ly:make-pitch -1 5 FLAT))
                (a . ,(ly:make-pitch -1 5 NATURAL))
                (aiss . ,(ly:make-pitch -1 5 SHARP))
                (aississ . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

                (hessess . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
                (b . ,(ly:make-pitch -1 6 FLAT))
                (h . ,(ly:make-pitch -1 6 NATURAL))
                (hiss . ,(ly:make-pitch -1 6 SHARP))
                (hississ . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
                ))


    ;; Language: Vlaams ------------------------------------------------;
    ;; Hendrik Maryns <hendrik.maryns@ugent.be>
    ;;
    ;;  b  = flat (bemol)
    ;;  bb = double-flat
    ;;
    ;;  k  = sharp (kruis)
    ;;  kk = double-sharp
    ;;
    ;;  English: c   d   e   f   g   a   b
    ;;  Flemish: do  re  mi  fa  sol la  si

    (vlaams . (
               (dobb . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
               (dob . ,(ly:make-pitch -1 0 FLAT))
               (do . ,(ly:make-pitch -1 0 NATURAL))
               (dok . ,(ly:make-pitch -1 0 SHARP))
               (dokk . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

               (rebb . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
               (reb . ,(ly:make-pitch -1 1 FLAT))
               (re . ,(ly:make-pitch -1 1 NATURAL))
               (rek . ,(ly:make-pitch -1 1 SHARP))
               (rekk . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

               (mibb . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
               (mib . ,(ly:make-pitch -1 2 FLAT))
               (mi . ,(ly:make-pitch -1 2 NATURAL))
               (mik . ,(ly:make-pitch -1 2 SHARP))
               (mikk . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

               (fabb . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
               (fab . ,(ly:make-pitch -1 3 FLAT))
               (fa . ,(ly:make-pitch -1 3 NATURAL))
               (fak . ,(ly:make-pitch -1 3 SHARP))
               (fakk . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

               (solbb . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
               (solb . ,(ly:make-pitch -1 4 FLAT))
               (sol . ,(ly:make-pitch -1 4 NATURAL))
               (solk . ,(ly:make-pitch -1 4 SHARP))
               (solkk . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

               (labb . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
               (lab . ,(ly:make-pitch -1 5 FLAT))
               (la . ,(ly:make-pitch -1 5 NATURAL))
               (lak . ,(ly:make-pitch -1 5 SHARP))
               (lakk . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

               (sibb . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
               (sib . ,(ly:make-pitch -1 6 FLAT))
               (si . ,(ly:make-pitch -1 6 NATURAL))
               (sik . ,(ly:make-pitch -1 6 SHARP))
               (sikk . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
               ))
    ))

;; add two native utf-8 aliases. Pairs obey cp-like order: '(old new)
(for-each
 (lambda (pair)
   (set! language-pitch-names
         (append language-pitch-names
                 (list (cons (cadr pair)
                             (cdr (assoc (car pair) language-pitch-names)))))))
 '((espanol español)))

(define-public (note-names-language str)
  (_ "Select note names language.")
  (let ((alist (assoc-get (string->symbol str)
                          language-pitch-names
                          '())))
    (if (pair? alist)
        (begin
          (ly:debug (_ "Using `~a' note names...") str)
          (set! pitchnames alist)
          (ly:parser-set-note-names alist))
        (ly:warning (_ "Could not find language `~a'.  Ignoring.") str))))
