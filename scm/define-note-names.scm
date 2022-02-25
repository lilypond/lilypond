;;;; Common note names in various languages.
;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2010--2022 Valentin Villenave <valentin@villenave.net> et al:
;;;;
;;;; Copyright (C) 1996--2022 Han-Wen Nienhuys <hanwen@xs4all.nl> (Nederlands)
;;;; Copyright (C) 1998--2022 Jaume Obrador <jobrador@ipc4.uib.es> (Catalan)
;;;; Copyright (C) 1997--2022 Roland Meier <meier@informatik.th-darmstadt.de>
;;;;                Bjoern Jacke <bjoern.jacke@gmx.de> (Deutsch)
;;;; Copyright (C) 1996--2022 Han-Wen Nienhuys <hanwen@xs4all.nl> (English)
;;;; Copyright (C) 2002--2022 Carlos García Suárez <cgscqmp@terra.es>
;;;;                Maximiliano G. G. <mxgdvg@yahoo.it> (Espanol)
;;;; Copyright (C) 1998--2022 Paolo Zuliani <zuliap@easynet.it>
;;;;                Eric Wurbel <wurbel@univ-tln.fr> (Italiano)
;;;; Copyright (C) 1998--2022 Arvid Grøtting <arvidg@ifi.uio.no> (Norsk)
;;;; Copyright (C) 2004--2022 Pedro Kröger <kroeger@pedrokroeger.net> (Portugues)
;;;; Copyright (C) 2001--2022 Heikki Junes <heikki.junes@hut.fi> (Suomi)
;;;; Copyright (C) 1997--2022 Mats Bengtsson <mabe@violin.s3.kth.se> (Svenska)
;;;; Copyright (C) 2004--2022 Hendrik Maryns <hendrik.maryns@ugent.be> (Vlaams)
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
(define-session-public pitchnames '())
(define-session-public default-language "")

(define-session-public language-pitch-names
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


    ;; Language: Català ------------------------------------------------;
    ;; Jaume Obrador <jobrador@ipc4.uib.es>
    ;; Torsten Hämmerle <torsten.haemmerle@web.de>  quarter-tones added
    ;;
    ;;  b   = flat (bemoll)
    ;;  bb  = double-flat
    ;;  qb  = quarter-tone flat (quart de to...)
    ;;  tqb = three quarter-tones flat (tres quarts de to...)
    ;;
    ;;  d   = sharp (diesi)
    ;;  dd  = double-sharp
    ;;  qd  = quarter-tone sharp (quart de to...)
    ;;  tqd = three-quarter-tones sharp (tres quarts de to...)
    ;;
    ;;  s   = sharp (sostingut)
    ;;  ss  = double-sharp
    ;;  qs  = quarter-tone sharp (quart de to...)
    ;;  tqs = three-quarter-tones sharp (tres quarts de to...)
    ;;
    ;;  English: c   d   e   f   g   a   b
    ;;  Catalan: do  re  mi  fa  sol la  si

    (català . (
               (dobb . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
               (dotqb . ,(ly:make-pitch -1 0 THREE-Q-FLAT))
               (dob . ,(ly:make-pitch -1 0 FLAT))
               (doqb . ,(ly:make-pitch -1 0 SEMI-FLAT))
               (do . ,(ly:make-pitch -1 0 NATURAL))
               (doqd . ,(ly:make-pitch -1 0 SEMI-SHARP))
               (dod . ,(ly:make-pitch -1 0 SHARP))
               (dotqd . ,(ly:make-pitch -1 0 THREE-Q-SHARP))
               (dodd . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

               (rebb . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
               (retqb . ,(ly:make-pitch -1 1 THREE-Q-FLAT))
               (reb . ,(ly:make-pitch -1 1 FLAT))
               (reqb . ,(ly:make-pitch -1 1 SEMI-FLAT))
               (re . ,(ly:make-pitch -1 1 NATURAL))
               (reqd . ,(ly:make-pitch -1 1 SEMI-SHARP))
               (red . ,(ly:make-pitch -1 1 SHARP))
               (retqd . ,(ly:make-pitch -1 1 THREE-Q-SHARP))
               (redd . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

               (mibb . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
               (mitqb . ,(ly:make-pitch -1 2 THREE-Q-FLAT))
               (mib . ,(ly:make-pitch -1 2 FLAT))
               (miqb . ,(ly:make-pitch -1 2 SEMI-FLAT))
               (mi . ,(ly:make-pitch -1 2 NATURAL))
               (miqd . ,(ly:make-pitch -1 2 SEMI-SHARP))
               (mid . ,(ly:make-pitch -1 2 SHARP))
               (mitqd . ,(ly:make-pitch -1 2 THREE-Q-SHARP))
               (midd . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

               (fabb . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
               (fatqb . ,(ly:make-pitch -1 3 THREE-Q-FLAT))
               (fab . ,(ly:make-pitch -1 3 FLAT))
               (faqb . ,(ly:make-pitch -1 3 SEMI-FLAT))
               (fa . ,(ly:make-pitch -1 3 NATURAL))
               (faqd . ,(ly:make-pitch -1 3 SEMI-SHARP))
               (fad . ,(ly:make-pitch -1 3 SHARP))
               (fatqd . ,(ly:make-pitch -1 3 THREE-Q-SHARP))
               (fadd . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

               (solbb . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
               (soltqb . ,(ly:make-pitch -1 4 THREE-Q-FLAT))
               (solb . ,(ly:make-pitch -1 4 FLAT))
               (solqb . ,(ly:make-pitch -1 4 SEMI-FLAT))
               (sol . ,(ly:make-pitch -1 4 NATURAL))
               (solqd . ,(ly:make-pitch -1 4 SEMI-SHARP))
               (sold . ,(ly:make-pitch -1 4 SHARP))
               (soltqd . ,(ly:make-pitch -1 4 THREE-Q-SHARP))
               (soldd . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

               (labb . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
               (latqb . ,(ly:make-pitch -1 5 THREE-Q-FLAT))
               (lab . ,(ly:make-pitch -1 5 FLAT))
               (laqb . ,(ly:make-pitch -1 5 SEMI-FLAT))
               (la . ,(ly:make-pitch -1 5 NATURAL))
               (laqd . ,(ly:make-pitch -1 5 SEMI-SHARP))
               (lad . ,(ly:make-pitch -1 5 SHARP))
               (latqd . ,(ly:make-pitch -1 5 THREE-Q-SHARP))
               (ladd . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

               (sibb . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
               (sitqb . ,(ly:make-pitch -1 6 THREE-Q-FLAT))
               (sib . ,(ly:make-pitch -1 6 FLAT))
               (siqb . ,(ly:make-pitch -1 6 SEMI-FLAT))
               (si . ,(ly:make-pitch -1 6 NATURAL))
               (siqd . ,(ly:make-pitch -1 6 SEMI-SHARP))
               (sid . ,(ly:make-pitch -1 6 SHARP))
               (sitqd . ,(ly:make-pitch -1 6 THREE-Q-SHARP))
               (sidd . ,(ly:make-pitch -1 6 DOUBLE-SHARP))

               ;; Now that we have espanol.ly, should these be junked? --jcn
               (doqs . ,(ly:make-pitch -1 0 SEMI-SHARP))
               (dos . ,(ly:make-pitch -1 0 SHARP))
               (dotqs . ,(ly:make-pitch -1 0 THREE-Q-SHARP))
               (doss . ,(ly:make-pitch -1 0 DOUBLE-SHARP))
               (reqs . ,(ly:make-pitch -1 1 SEMI-SHARP))
               (res . ,(ly:make-pitch -1 1 SHARP))
               (retqs . ,(ly:make-pitch -1 1 THREE-Q-SHARP))
               (ress . ,(ly:make-pitch -1 1 DOUBLE-SHARP))
               (miqs . ,(ly:make-pitch -1 2 SEMI-SHARP))
               (mis . ,(ly:make-pitch -1 2 SHARP))
               (mitqs . ,(ly:make-pitch -1 2 THREE-Q-SHARP))
               (miss . ,(ly:make-pitch -1 2 DOUBLE-SHARP))
               (faqs . ,(ly:make-pitch -1 3 SEMI-SHARP))
               (fas . ,(ly:make-pitch -1 3 SHARP))
               (fatqs . ,(ly:make-pitch -1 3 THREE-Q-SHARP))
               (fass . ,(ly:make-pitch -1 3 DOUBLE-SHARP))
               (solqs . ,(ly:make-pitch -1 4 SEMI-SHARP))
               (sols . ,(ly:make-pitch -1 4 SHARP))
               (soltqs . ,(ly:make-pitch -1 4 THREE-Q-SHARP))
               (solss . ,(ly:make-pitch -1 4 DOUBLE-SHARP))
               (laqs . ,(ly:make-pitch -1 5 SEMI-SHARP))
               (las . ,(ly:make-pitch -1 5 SHARP))
               (latqs . ,(ly:make-pitch -1 5 THREE-Q-SHARP))
               (lass . ,(ly:make-pitch -1 5 DOUBLE-SHARP))
               (siqs . ,(ly:make-pitch -1 6 SEMI-SHARP))
               (sis . ,(ly:make-pitch -1 6 SHARP))
               (sitqs . ,(ly:make-pitch -1 6 THREE-Q-SHARP))
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
                (eeh . ,(ly:make-pitch -1 2 SEMI-FLAT)) ;; should be eh; kept for backward compatibility
                (eh . ,(ly:make-pitch -1 2 SEMI-FLAT))
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
                (aeh . ,(ly:make-pitch -1 5 SEMI-FLAT)) ;; should be ah; kepy for backward compatibility
                (ah . ,(ly:make-pitch -1 5 SEMI-FLAT))
                (a . ,(ly:make-pitch -1 5 NATURAL))
                (aih . ,(ly:make-pitch -1 5 SEMI-SHARP))
                (ais . ,(ly:make-pitch -1 5 SHARP))
                (aisih . ,(ly:make-pitch -1 5 THREE-Q-SHARP))
                (aisis . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

                (heses . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
                (heseh . ,(ly:make-pitch -1 6 THREE-Q-FLAT))
                (b . ,(ly:make-pitch -1 6 FLAT))
                (heh . ,(ly:make-pitch -1 6 SEMI-FLAT))
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


    ;; Language: Español -----------------------------------------------;
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

    (español . (
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
    ;; Torsten Hämmerle <torsten.haemmerle@web.de>  quarter-tones added
    ;;
    ;;  es     = flat
    ;;  ess    = flat
    ;;  eses   = double-flat
    ;;  essess = double-flat
    ;;  eh     = quarter-tone flat
    ;;  eseh   = three quarter-tones flat
    ;;  esseh  = three quarter-tones flat
    ;;
    ;;  is     = sharp
    ;;  iss    = sharp
    ;;  isis   = double-sharp
    ;;  ississ = double-sharp
    ;;  ih     = quarter-tone sharp
    ;;  isih   = three quarter-tones sharp
    ;;  issih  = three quarter-tones sharp
    ;;
    ;;    English: c  d  e  f  g  a  bf b
    ;;  Norwegian: c  d  e  f  g  a  b  h

    (norsk . (
              (cessess . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
              (ceses . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
              (cesseh . ,(ly:make-pitch -1 0 THREE-Q-FLAT))
              (ceseh . ,(ly:make-pitch -1 0 THREE-Q-FLAT))
              (cess . ,(ly:make-pitch -1 0 FLAT))
              (ces . ,(ly:make-pitch -1 0 FLAT))
              (ceh . ,(ly:make-pitch -1 0 SEMI-FLAT))
              (c . ,(ly:make-pitch -1 0 NATURAL))
              (cih . ,(ly:make-pitch -1 0 SEMI-SHARP))
              (ciss . ,(ly:make-pitch -1 0 SHARP))
              (cis . ,(ly:make-pitch -1 0 SHARP))
              (cissih . ,(ly:make-pitch -1 0 THREE-Q-SHARP))
              (cisih . ,(ly:make-pitch -1 0 THREE-Q-SHARP))
              (cississ . ,(ly:make-pitch -1 0 DOUBLE-SHARP))
              (cisis . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

              (dessess . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
              (deses . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
              (desseh . ,(ly:make-pitch -1 1 THREE-Q-FLAT))
              (deseh . ,(ly:make-pitch -1 1 THREE-Q-FLAT))
              (dess . ,(ly:make-pitch -1 1 FLAT))
              (des . ,(ly:make-pitch -1 1 FLAT))
              (deh . ,(ly:make-pitch -1 1 SEMI-FLAT))
              (d . ,(ly:make-pitch -1 1 NATURAL))
              (dih . ,(ly:make-pitch -1 1 SEMI-SHARP))
              (diss . ,(ly:make-pitch -1 1 SHARP))
              (dis . ,(ly:make-pitch -1 1 SHARP))
              (dissih . ,(ly:make-pitch -1 1 THREE-Q-SHARP))
              (disih . ,(ly:make-pitch -1 1 THREE-Q-SHARP))
              (dississ . ,(ly:make-pitch -1 1 DOUBLE-SHARP))
              (disis . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

              (eessess . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
              (eeses . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
              (essess . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
              (eses . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
              (eesseh . ,(ly:make-pitch -1 2 THREE-Q-FLAT))
              (eeseh . ,(ly:make-pitch -1 2 THREE-Q-FLAT))
              (eess . ,(ly:make-pitch -1 2 FLAT))
              (ees . ,(ly:make-pitch -1 2 FLAT))
              (ess . ,(ly:make-pitch -1 2 FLAT))
              (es . ,(ly:make-pitch -1 2 FLAT))
              (eeh . ,(ly:make-pitch -1 2 SEMI-FLAT))
              (e . ,(ly:make-pitch -1 2 NATURAL))
              (eih . ,(ly:make-pitch -1 2 SEMI-SHARP))
              (eiss . ,(ly:make-pitch -1 2 SHARP))
              (eis . ,(ly:make-pitch -1 2 SHARP))
              (eissih . ,(ly:make-pitch -1 2 THREE-Q-SHARP))
              (eisih . ,(ly:make-pitch -1 2 THREE-Q-SHARP))
              (eississ . ,(ly:make-pitch -1 2 DOUBLE-SHARP))
              (eisis . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

              (fessess . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
              (feses . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
              (fesseh . ,(ly:make-pitch -1 3 THREE-Q-FLAT))
              (feseh . ,(ly:make-pitch -1 3 THREE-Q-FLAT))
              (fess . ,(ly:make-pitch -1 3 FLAT))
              (fes . ,(ly:make-pitch -1 3 FLAT))
              (feh . ,(ly:make-pitch -1 3 SEMI-FLAT))
              (f . ,(ly:make-pitch -1 3 NATURAL))
              (fih . ,(ly:make-pitch -1 3 SEMI-SHARP))
              (fiss . ,(ly:make-pitch -1 3 SHARP))
              (fis . ,(ly:make-pitch -1 3 SHARP))
              (fissih . ,(ly:make-pitch -1 3 THREE-Q-SHARP))
              (fisih . ,(ly:make-pitch -1 3 THREE-Q-SHARP))
              (fississ . ,(ly:make-pitch -1 3 DOUBLE-SHARP))
              (fisis . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

              (gessess . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
              (geses . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
              (geseh . ,(ly:make-pitch -1 4 THREE-Q-FLAT))
              (gesseh . ,(ly:make-pitch -1 4 THREE-Q-FLAT))
              (gess . ,(ly:make-pitch -1 4 FLAT))
              (ges . ,(ly:make-pitch -1 4 FLAT))
              (geh . ,(ly:make-pitch -1 4 SEMI-FLAT))
              (g . ,(ly:make-pitch -1 4 NATURAL))
              (gih . ,(ly:make-pitch -1 4 SEMI-SHARP))
              (giss . ,(ly:make-pitch -1 4 SHARP))
              (gis . ,(ly:make-pitch -1 4 SHARP))
              (gissih . ,(ly:make-pitch -1 4 THREE-Q-SHARP))
              (gisih . ,(ly:make-pitch -1 4 THREE-Q-SHARP))
              (gississ . ,(ly:make-pitch -1 4 DOUBLE-SHARP))
              (gisis . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

              (assess . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
              (ases . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
              (aessess . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
              (aeses . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
              (aesseh . ,(ly:make-pitch -1 5 THREE-Q-FLAT))
              (aeseh . ,(ly:make-pitch -1 5 THREE-Q-FLAT))
              (ass . ,(ly:make-pitch -1 5 FLAT))
              (as . ,(ly:make-pitch -1 5 FLAT))
              (aess . ,(ly:make-pitch -1 5 FLAT))
              (aes . ,(ly:make-pitch -1 5 FLAT))
              (aeh . ,(ly:make-pitch -1 5 SEMI-FLAT))
              (a . ,(ly:make-pitch -1 5 NATURAL))
              (aih . ,(ly:make-pitch -1 5 SEMI-SHARP))
              (aiss . ,(ly:make-pitch -1 5 SHARP))
              (ais . ,(ly:make-pitch -1 5 SHARP))
              (aissih . ,(ly:make-pitch -1 5 THREE-Q-SHARP))
              (aisih . ,(ly:make-pitch -1 5 THREE-Q-SHARP))
              (aississ . ,(ly:make-pitch -1 5 DOUBLE-SHARP))
              (aisis . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

              (bess . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
              (bes . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
              (beh . ,(ly:make-pitch -1 6 THREE-Q-FLAT))
              (b . ,(ly:make-pitch -1 6 FLAT))
              (heh . ,(ly:make-pitch -1 6 SEMI-FLAT))
              (h . ,(ly:make-pitch -1 6 NATURAL))
              (hih . ,(ly:make-pitch -1 6 SEMI-SHARP))
              (hiss . ,(ly:make-pitch -1 6 SHARP))
              (his . ,(ly:make-pitch -1 6 SHARP))
              (hissih . ,(ly:make-pitch -1 6 THREE-Q-SHARP))
              (hisih . ,(ly:make-pitch -1 6 THREE-Q-SHARP))
              (hississ . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
              (hisis . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
              ))


    ;; Language: Português ---------------------------------------------;
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

    (português . (
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
    ;; Torsten Hämmerle <torsten.haemmerle@web.de>  quarter-tones added
    ;;
    ;;  es   = flat
    ;;  eses = double-flat
    ;;  eh   = quarter-tone flat
    ;;  eseh = three quarter-tones flat
    ;;
    ;;  is   = sharp
    ;;  isis = double-sharp
    ;;  ih   = quarter-tone sharp
    ;;  isih = three quarter-tones sharp
    ;;
    ;;  English: c  d  e  f  g  a  bf b
    ;;  Finnish: c  d  e  f  g  a  b  h

    (suomi . (
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
              (aseh . ,(ly:make-pitch -1 5 THREE-Q-FLAT))
              (as . ,(ly:make-pitch -1 5 FLAT))
              (aeh . ,(ly:make-pitch -1 5 SEMI-FLAT))
              (a . ,(ly:make-pitch -1 5 NATURAL))
              (aih . ,(ly:make-pitch -1 5 SEMI-SHARP))
              (ais . ,(ly:make-pitch -1 5 SHARP))
              (aisih . ,(ly:make-pitch -1 5 THREE-Q-SHARP))
              (aisis . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

              (heses . ,(ly:make-pitch -1 6 DOUBLE-FLAT))  ;; non-standard name for bb
              (bb . ,(ly:make-pitch -1 6 DOUBLE-FLAT)) ;; should be bes; kept for backward compatibility
              (bes . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
              (heseh . ,(ly:make-pitch -1 6 THREE-Q-FLAT))
              (b . ,(ly:make-pitch -1 6 FLAT))
              (heh . ,(ly:make-pitch -1 6 SEMI-FLAT))
              (h . ,(ly:make-pitch -1 6 NATURAL))
              (hih . ,(ly:make-pitch -1 6 SEMI-SHARP))
              (his . ,(ly:make-pitch -1 6 SHARP))
              (hisih . ,(ly:make-pitch -1 6 THREE-Q-SHARP))
              (hisis . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
              ))


    ;; Language: Svenska -----------------------------------------------;
    ;; Mats Bengtsson <mabe@violin.s3.kth.se>
    ;; Torsten Hämmerle <torsten.haemmerle@web.de>  quarter-tones added
    ;;
    ;;  ess    = flat
    ;;  essess = double-flat
    ;;  eh        = quarter-tone flat
    ;;  esseh     = three quarter-tones flat
    ;;
    ;;  iss    = sharp
    ;;  ississ = double-sharp
    ;;  ih        = quarter-tone sharp
    ;;  issih     = three quarter-tones sharp
    ;;
    ;;  English: c  d  e  f  g  a  bf b
    ;;  Swedish: c  d  e  f  g  a  b  h

    (svenska . (
                (cessess . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
                (cesseh . ,(ly:make-pitch -1 0 THREE-Q-FLAT))
                (cess . ,(ly:make-pitch -1 0 FLAT))
                (ceh . ,(ly:make-pitch -1 0 SEMI-FLAT))
                (c . ,(ly:make-pitch -1 0 NATURAL))
                (cih . ,(ly:make-pitch -1 0 SEMI-SHARP))
                (ciss . ,(ly:make-pitch -1 0 SHARP))
                (cissih . ,(ly:make-pitch -1 0 THREE-Q-SHARP))
                (cississ . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

                (dessess . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
                (desseh . ,(ly:make-pitch -1 1 THREE-Q-FLAT))
                (dess . ,(ly:make-pitch -1 1 FLAT))
                (deh . ,(ly:make-pitch -1 1 SEMI-FLAT))
                (d . ,(ly:make-pitch -1 1 NATURAL))
                (dih . ,(ly:make-pitch -1 1 SEMI-SHARP))
                (diss . ,(ly:make-pitch -1 1 SHARP))
                (dissih . ,(ly:make-pitch -1 1 THREE-Q-SHARP))
                (dississ . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

                (essess . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
                (esseh . ,(ly:make-pitch -1 2 THREE-Q-FLAT))
                (ess . ,(ly:make-pitch -1 2 FLAT))
                (eeh . ,(ly:make-pitch -1 2 SEMI-FLAT))
                (e . ,(ly:make-pitch -1 2 NATURAL))
                (eih . ,(ly:make-pitch -1 2 SEMI-SHARP))
                (eiss . ,(ly:make-pitch -1 2 SHARP))
                (eissih . ,(ly:make-pitch -1 2 THREE-Q-SHARP))
                (eississ . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

                (fessess . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
                (fesseh . ,(ly:make-pitch -1 3 THREE-Q-FLAT))
                (fess . ,(ly:make-pitch -1 3 FLAT))
                (feh . ,(ly:make-pitch -1 3 SEMI-FLAT))
                (f . ,(ly:make-pitch -1 3 NATURAL))
                (fih . ,(ly:make-pitch -1 3 SEMI-SHARP))
                (fiss . ,(ly:make-pitch -1 3 SHARP))
                (fissih . ,(ly:make-pitch -1 3 THREE-Q-SHARP))
                (fississ . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

                (gessess . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
                (gesseh . ,(ly:make-pitch -1 4 THREE-Q-FLAT))
                (gess . ,(ly:make-pitch -1 4 FLAT))
                (geh . ,(ly:make-pitch -1 4 SEMI-FLAT))
                (g . ,(ly:make-pitch -1 4 NATURAL))
                (gih . ,(ly:make-pitch -1 4 SEMI-SHARP))
                (giss . ,(ly:make-pitch -1 4 SHARP))
                (gissih . ,(ly:make-pitch -1 4 THREE-Q-SHARP))
                (gississ . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

                (assess . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
                (asseh . ,(ly:make-pitch -1 5 THREE-Q-FLAT))
                (ass . ,(ly:make-pitch -1 5 FLAT))
                (aeh . ,(ly:make-pitch -1 5 SEMI-FLAT))
                (a . ,(ly:make-pitch -1 5 NATURAL))
                (aih . ,(ly:make-pitch -1 5 SEMI-SHARP))
                (aiss . ,(ly:make-pitch -1 5 SHARP))
                (aissih . ,(ly:make-pitch -1 5 THREE-Q-SHARP))
                (aississ . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

                (hessess . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
                (hesseh . ,(ly:make-pitch -1 6 THREE-Q-FLAT))
                (b . ,(ly:make-pitch -1 6 FLAT))
                (heh . ,(ly:make-pitch -1 6 SEMI-FLAT))
                (h . ,(ly:make-pitch -1 6 NATURAL))
                (hih . ,(ly:make-pitch -1 6 SEMI-SHARP))
                (hiss . ,(ly:make-pitch -1 6 SHARP))
                (hissih . ,(ly:make-pitch -1 6 THREE-Q-SHARP))
                (hississ . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
                ))


    ;; Language: Vlaams ------------------------------------------------;
    ;; Hendrik Maryns <hendrik.maryns@ugent.be>
    ;; Torsten Hämmerle <torsten.haemmerle@web.de>  quarter-tones added
    ;;
    ;;  b   = flat (bemol)
    ;;  bb  = double-flat
    ;;  hb  = quarter-tone flat (halve bemol)
    ;;  bhb = three quarter-tones flat
    ;;
    ;;  k   = sharp (kruis)
    ;;  kk  = double-sharp
    ;;  hk  = quarter-tone sharp (halve kruis)
    ;;  khk = three quarter-tones sharp
    ;;
    ;;  English: c   d   e   f   g   a   b
    ;;  Flemish: do  re  mi  fa  sol la  si

    (vlaams . (
               (dobb . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
               (dobhb . ,(ly:make-pitch -1 0 THREE-Q-FLAT))
               (dob . ,(ly:make-pitch -1 0 FLAT))
               (dohb . ,(ly:make-pitch -1 0 SEMI-FLAT))
               (do . ,(ly:make-pitch -1 0 NATURAL))
               (dohk . ,(ly:make-pitch -1 0 SEMI-SHARP))
               (dok . ,(ly:make-pitch -1 0 SHARP))
               (dokhk . ,(ly:make-pitch -1 0 THREE-Q-SHARP))
               (dokk . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

               (rebb . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
               (rebhb . ,(ly:make-pitch -1 1 THREE-Q-FLAT))
               (reb . ,(ly:make-pitch -1 1 FLAT))
               (rehb . ,(ly:make-pitch -1 1 SEMI-FLAT))
               (re . ,(ly:make-pitch -1 1 NATURAL))
               (rehk . ,(ly:make-pitch -1 1 SEMI-SHARP))
               (rek . ,(ly:make-pitch -1 1 SHARP))
               (rekhk . ,(ly:make-pitch -1 1 THREE-Q-SHARP))
               (rekk . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

               (mibb . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
               (mibhb . ,(ly:make-pitch -1 2 THREE-Q-FLAT))
               (mib . ,(ly:make-pitch -1 2 FLAT))
               (mihb . ,(ly:make-pitch -1 2 SEMI-FLAT))
               (mi . ,(ly:make-pitch -1 2 NATURAL))
               (mihk . ,(ly:make-pitch -1 2 SEMI-SHARP))
               (mik . ,(ly:make-pitch -1 2 SHARP))
               (mikhk . ,(ly:make-pitch -1 2 THREE-Q-SHARP))
               (mikk . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

               (fabb . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
               (fabhb . ,(ly:make-pitch -1 3 THREE-Q-FLAT))
               (fab . ,(ly:make-pitch -1 3 FLAT))
               (fahb . ,(ly:make-pitch -1 3 SEMI-FLAT))
               (fa . ,(ly:make-pitch -1 3 NATURAL))
               (fahk . ,(ly:make-pitch -1 3 SEMI-SHARP))
               (fak . ,(ly:make-pitch -1 3 SHARP))
               (fakhk . ,(ly:make-pitch -1 3 THREE-Q-SHARP))
               (fakk . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

               (solbb . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
               (solbhb . ,(ly:make-pitch -1 4 THREE-Q-FLAT))
               (solb . ,(ly:make-pitch -1 4 FLAT))
               (solhb . ,(ly:make-pitch -1 4 SEMI-FLAT))
               (sol . ,(ly:make-pitch -1 4 NATURAL))
               (solhk . ,(ly:make-pitch -1 4 SEMI-SHARP))
               (solk . ,(ly:make-pitch -1 4 SHARP))
               (solkhk . ,(ly:make-pitch -1 4 THREE-Q-SHARP))
               (solkk . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

               (labb . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
               (labhb . ,(ly:make-pitch -1 5 THREE-Q-FLAT))
               (lab . ,(ly:make-pitch -1 5 FLAT))
               (lahb . ,(ly:make-pitch -1 5 SEMI-FLAT))
               (la . ,(ly:make-pitch -1 5 NATURAL))
               (lahk . ,(ly:make-pitch -1 5 SEMI-SHARP))
               (lak . ,(ly:make-pitch -1 5 SHARP))
               (lakhk . ,(ly:make-pitch -1 5 THREE-Q-SHARP))
               (lakk . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

               (sibb . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
               (sibhb . ,(ly:make-pitch -1 6 THREE-Q-FLAT))
               (sib . ,(ly:make-pitch -1 6 FLAT))
               (sihb . ,(ly:make-pitch -1 6 SEMI-FLAT))
               (si . ,(ly:make-pitch -1 6 NATURAL))
               (sihk . ,(ly:make-pitch -1 6 SEMI-SHARP))
               (sik . ,(ly:make-pitch -1 6 SHARP))
               (sikhk . ,(ly:make-pitch -1 6 THREE-Q-SHARP))
               (sikk . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
               ))


    ;; Language: Arabic ------------------------------------------------;
    ;; Hassan El fatihi <hassan.elfatihi@free.fr>,
    ;;
    ;;  bb   = flat (bemol)
    ;;  bb  = double-flat
    ;;  db  = quarter-tone flat (semi-bemol)
    ;;  dd  = quarter-tone sharp (semi-diesis)
    ;;  ctqb = three-quarter-tones flat
    ;;  ctqd = three-quarter-tones sharp
    ;;  d   = sharp (diesis)
    ;;  dd  = double-sharp
    ;;  cfhf = seven-half-tones flat
    ;;  cfhd = seven-half-tones sharp
    ;;  cshb = seven-half-tones flat
    ;;  cshd = seven-half-tones sharp
    ;;  English: c   d   e   f   g   a   b
    ;;  Italian: do  re  mi  fa  sol la  si

    (arabic . (
               (c . ,(ly:make-pitch -1 0 NATURAL))
               (d . ,(ly:make-pitch -1 1 NATURAL))
               (e . ,(ly:make-pitch -1 2 NATURAL))
               (f . ,(ly:make-pitch -1 3 NATURAL))
               (g . ,(ly:make-pitch -1 4 NATURAL))
               (a . ,(ly:make-pitch -1 5 NATURAL))
               (b . ,(ly:make-pitch -1 6 NATURAL))

               (cd . ,(ly:make-pitch -1 0 SHARP))
               (dd . ,(ly:make-pitch -1 1 SHARP))
               (ed . ,(ly:make-pitch -1 2 SHARP))
               (fd . ,(ly:make-pitch -1 3 SHARP))
               (gd . ,(ly:make-pitch -1 4 SHARP))
               (ad . ,(ly:make-pitch -1 5 SHARP))
               (bd . ,(ly:make-pitch -1 6 SHARP))

               (cb . ,(ly:make-pitch -1 0 FLAT))
               (db . ,(ly:make-pitch -1 1 FLAT))
               (eb . ,(ly:make-pitch -1 2 FLAT))
               (fb . ,(ly:make-pitch -1 3 FLAT))
               (gb . ,(ly:make-pitch -1 4 FLAT))
               (ab . ,(ly:make-pitch -1 5 FLAT))
               (bb . ,(ly:make-pitch -1 6 FLAT))

               (cdd . ,(ly:make-pitch -1 0 SEMI-SHARP))
               (ddd . ,(ly:make-pitch -1 1 SEMI-SHARP))
               (edd . ,(ly:make-pitch -1 2 SEMI-SHARP))
               (fdd . ,(ly:make-pitch -1 3 SEMI-SHARP))
               (gdd . ,(ly:make-pitch -1 4 SEMI-SHARP))
               (add . ,(ly:make-pitch -1 5 SEMI-SHARP))
               (bdd . ,(ly:make-pitch -1 6 SEMI-SHARP))

               (cdb . ,(ly:make-pitch -1 0 SEMI-FLAT))
               (ddb . ,(ly:make-pitch -1 1 SEMI-FLAT))
               (edb . ,(ly:make-pitch -1 2 SEMI-FLAT))
               (fdb . ,(ly:make-pitch -1 3 SEMI-FLAT))
               (gdb . ,(ly:make-pitch -1 4 SEMI-FLAT))
               (adb . ,(ly:make-pitch -1 5 SEMI-FLAT))
               (bdb . ,(ly:make-pitch -1 6 SEMI-FLAT))

               (ctqb . ,(ly:make-pitch -1 0 THREE-Q-FLAT))
               (dtqb . ,(ly:make-pitch -1 1 THREE-Q-FLAT))
               (etqb . ,(ly:make-pitch -1 2 THREE-Q-FLAT))
               (ftqb . ,(ly:make-pitch -1 3 THREE-Q-FLAT))
               (gtqb . ,(ly:make-pitch -1 4 THREE-Q-FLAT))
               (atqb . ,(ly:make-pitch -1 5 THREE-Q-FLAT))
               (btqb . ,(ly:make-pitch -1 6 THREE-Q-FLAT))

               (ctqd . ,(ly:make-pitch -1 0 THREE-Q-SHARP))
               (dtqd . ,(ly:make-pitch -1 1 THREE-Q-SHARP))
               (etqd . ,(ly:make-pitch -1 2 THREE-Q-SHARP))
               (ftqd . ,(ly:make-pitch -1 3 THREE-Q-SHARP))
               (gtqd . ,(ly:make-pitch -1 4 THREE-Q-SHARP))
               (atqd . ,(ly:make-pitch -1 5 THREE-Q-SHARP))
               (btqd . ,(ly:make-pitch -1 6 THREE-Q-SHARP))

               (cfhb . ,(ly:make-pitch -1 0 FIVE-HALF-FLAT))
               (dfhb . ,(ly:make-pitch -1 1 FIVE-HALF-FLAT))
               (efhb . ,(ly:make-pitch -1 2 FIVE-HALF-FLAT))
               (ffhb . ,(ly:make-pitch -1 3 FIVE-HALF-FLAT))
               (gfhb . ,(ly:make-pitch -1 4 FIVE-HALF-FLAT))
               (afhb . ,(ly:make-pitch -1 5 FIVE-HALF-FLAT))
               (bfhb . ,(ly:make-pitch -1 6 FIVE-HALF-FLAT))

               (cfhd . ,(ly:make-pitch -1 0 FIVE-HALF-SHARP))
               (dfhd . ,(ly:make-pitch -1 1 FIVE-HALF-SHARP))
               (efhd . ,(ly:make-pitch -1 2 FIVE-HALF-SHARP))
               (ffhd . ,(ly:make-pitch -1 3 FIVE-HALF-SHARP))
               (gfhd . ,(ly:make-pitch -1 4 FIVE-HALF-SHARP))
               (afhd . ,(ly:make-pitch -1 5 FIVE-HALF-SHARP))
               (bfhd . ,(ly:make-pitch -1 6 FIVE-HALF-SHARP))

               (cshb . ,(ly:make-pitch -1 0 SEVEN-HALF-FLAT))
               (dshb . ,(ly:make-pitch -1 1 SEVEN-HALF-FLAT))
               (eshb . ,(ly:make-pitch -1 2 SEVEN-HALF-FLAT))
               (fshb . ,(ly:make-pitch -1 3 SEVEN-HALF-FLAT))
               (gshb . ,(ly:make-pitch -1 4 SEVEN-HALF-FLAT))
               (ashb . ,(ly:make-pitch -1 5 SEVEN-HALF-FLAT))
               (bshb . ,(ly:make-pitch -1 6 SEVEN-HALF-FLAT))

               (cshd . ,(ly:make-pitch -1 0 SEVEN-HALF-SHARP))
               (dshd . ,(ly:make-pitch -1 1 SEVEN-HALF-SHARP))
               (eshd . ,(ly:make-pitch -1 2 SEVEN-HALF-SHARP))
               (fshd . ,(ly:make-pitch -1 3 SEVEN-HALF-SHARP))
               (gshd . ,(ly:make-pitch -1 4 SEVEN-HALF-SHARP))
               (ashd . ,(ly:make-pitch -1 5 SEVEN-HALF-SHARP))
               (bshd . ,(ly:make-pitch -1 6 SEVEN-HALF-SHARP))
               ))
    ))


;; add native utf-8 aliases. Pairs obey cp-like order: '(old new)
(for-each
 (lambda (pair)
   (set! language-pitch-names
         (append language-pitch-names
                 (list (cons (cadr pair)
                             (cdr (assoc (car pair) language-pitch-names)))))))
 '((català catalan) (español espanol) (português portugues)))

(define-public (note-names-language str)
  (G_ "Select note names language.")
  (let ((alist (assoc-get (string->symbol str)
                          language-pitch-names
                          '())))
    (if (pair? alist)
        (begin
          (ly:debug (G_ "Using `~a' note names...") str)
          (ly:parser-set-note-names alist))
        (ly:warning (G_ "Could not find language `~a'.  Ignoring.") str))))
