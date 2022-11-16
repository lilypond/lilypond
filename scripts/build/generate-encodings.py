# generate-encodings.py
# -*- coding: utf-8 -*-
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2015--2022 Knut Petersen <knut_petersen@t-online.de>,
#               2018--2019 Malte Meyn <lilypond@maltemeyn.de>,
#               2022 Werner Lemberg  <wl@gnu.org>
#
# LilyPond is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# LilyPond is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.


# This file generates two other files that take care of the PostScript
# font encodings of the 'Emmentaler' and 'Emmentaler-Brace' fonts.
#
# ```
# python generate-encoding.py > encodingdefs.ps
# python generate-encoding.py -s > font-encodings.scm
# ```
#
# `encodingdefs.ps` is a PostScript resource file that defines the
# necessary font encodings and font name commands.  It gets included
# in the output if you use the `--ps` or `--eps` command line option
# of `lilypond`.
#
# `font-encodings.scm` provides a hash to quickly find the proper
# encoding vector for a given Emmentaler glyph name; it is used in
# function `named-glyph` (in file `output-ps.scm`).


import sys
import argparse


class LilypondGlyphset:
    def __init__(self, _suffix, _glyphset):
        self.suffix = _suffix
        self.glyphset = _glyphset


emmentaler_glyphsets = [
    LilypondGlyphset("N", [
        # standard and modern noteheads
        "noteheads.s0",
        "noteheads.s1",
        "noteheads.s2",
        "noteheads.sM1",
        "noteheads.sM1double",
        "noteheads.dM2",
        "noteheads.uM2",

        "noteheads.s0harmonic",
        "noteheads.s2harmonic",

        "noteheads.s0cross",
        "noteheads.s1cross",
        "noteheads.s2cross",

        "noteheads.s0diamond",
        "noteheads.s1diamond",
        "noteheads.s2diamond",

        "noteheads.s0triangle",
        "noteheads.s1triangle",
        "noteheads.s2triangle",

        "noteheads.s0slash",
        "noteheads.s1slash",
        "noteheads.s2slash",

        "noteheads.s2xcircle",

        # ancient noteheads
        "noteheads.s0mensural",
        "noteheads.s1mensural",
        "noteheads.s2mensural",
        "noteheads.sM1mensural",
        "noteheads.dM2mensural",
        "noteheads.uM2mensural",
        "noteheads.dM3mensural",
        "noteheads.uM3mensural",
        "noteheads.srM1mensural",
        "noteheads.drM2mensural",
        "noteheads.urM2mensural",
        "noteheads.drM3mensural",
        "noteheads.urM3mensural",

        "noteheads.sM2ligmensural",
        "noteheads.sM3ligmensural",
        "noteheads.srM2ligmensural",
        "noteheads.srM3ligmensural",

        "noteheads.s0neomensural",
        "noteheads.s1neomensural",
        "noteheads.s2neomensural",
        "noteheads.sM1neomensural",
        "noteheads.dM2neomensural",
        "noteheads.uM2neomensural",
        "noteheads.dM3neomensural",
        "noteheads.uM3neomensural",
        "noteheads.srM1neomensural",
        "noteheads.drM2neomensural",
        "noteheads.urM2neomensural",
        "noteheads.drM3neomensural",
        "noteheads.urM3neomensural",

        "noteheads.sM1semimensural",
        "noteheads.dM2semimensural",
        "noteheads.uM2semimensural",
        "noteheads.dM3semimensural",
        "noteheads.uM3semimensural",
        "noteheads.srM1semimensural",
        "noteheads.drM2semimensural",
        "noteheads.urM2semimensural",
        "noteheads.drM3semimensural",
        "noteheads.urM3semimensural",

        "noteheads.sM2semiligmensural",
        "noteheads.sM3semiligmensural",
        "noteheads.srM2semiligmensural",
        "noteheads.srM3semiligmensural",

        "noteheads.s0blackmensural",
        "noteheads.sM1blackmensural",
        "noteheads.dM2blackmensural",
        "noteheads.uM2blackmensural",
        "noteheads.dM3blackmensural",
        "noteheads.uM3blackmensural",

        "noteheads.sM2blackligmensural",
        "noteheads.sM3blackligmensural",

        "noteheads.s0petrucci",
        "noteheads.s1petrucci",
        "noteheads.s2petrucci",

        "noteheads.s0blackpetrucci",
        "noteheads.s1blackpetrucci",
        "noteheads.s2blackpetrucci",

        "noteheads.s0kievan",
        "noteheads.s1kievan",
        "noteheads.d2kievan",
        "noteheads.u2kievan",
        "noteheads.d3kievan",
        "noteheads.u3kievan",
        "noteheads.sM1kievan",
        "noteheads.sM2kievan",
        "noteheads.sr1kievan",

        # noteheads for Gregorian chant
        "noteheads.svaticana.punctum",
        "noteheads.svaticana.punctum.cavum",
        "noteheads.svaticana.cephalicus",
        "noteheads.svaticana.inner.cephalicus",
        "noteheads.svaticana.epiphonus",
        "noteheads.svaticana.vepiphonus",
        "noteheads.svaticana.inclinatum",
        "noteheads.svaticana.linea.punctum",
        "noteheads.svaticana.linea.punctum.cavum",
        "noteheads.svaticana.lpes",
        "noteheads.svaticana.vlpes",
        "noteheads.svaticana.plica",
        "noteheads.svaticana.reverse.plica",
        "noteheads.svaticana.vplica",
        "noteheads.svaticana.reverse.vplica",
        "noteheads.svaticana.quilisma",
        "noteheads.svaticana.upes",
        "noteheads.svaticana.vupes",

        "noteheads.ssolesmes.auct.asc",
        "noteheads.ssolesmes.auct.desc",
        "noteheads.ssolesmes.incl.auctum",
        "noteheads.ssolesmes.incl.parvum",
        "noteheads.ssolesmes.oriscus",
        "noteheads.ssolesmes.stropha",
        "noteheads.ssolesmes.stropha.aucta",

        "noteheads.shufnagel.lpes",
        "noteheads.shufnagel.punctum",
        "noteheads.shufnagel.virga",

        "noteheads.smedicaea.inclinatum",
        "noteheads.smedicaea.punctum",
        "noteheads.smedicaea.rvirga",
        "noteheads.smedicaea.virga",

        # noteheads for shape notes
        "noteheads.s0do",
        "noteheads.s0re",
        "noteheads.s0mi",
        "noteheads.s0miMirror",
        "noteheads.d0fa",
        "noteheads.u0fa",
        "noteheads.s0sol",
        "noteheads.s0la",
        "noteheads.s0ti",

        "noteheads.s1do",
        "noteheads.s1re",
        "noteheads.s1mi",
        "noteheads.s1miMirror",
        "noteheads.d1fa",
        "noteheads.u1fa",
        "noteheads.s1sol",
        "noteheads.s1la",
        "noteheads.s1ti",

        "noteheads.s2do",
        "noteheads.s2re",
        "noteheads.s2mi",
        "noteheads.s2miMirror",
        "noteheads.d2fa",
        "noteheads.u2fa",
        "noteheads.s2sol",
        "noteheads.s2la",
        "noteheads.s2ti",

        "noteheads.d0doFunk",
        "noteheads.u0doFunk",
        "noteheads.d0reFunk",
        "noteheads.u0reFunk",
        "noteheads.d0miFunk",
        "noteheads.u0miFunk",
        "noteheads.d0faFunk",
        "noteheads.u0faFunk",
        "noteheads.s0solFunk",
        "noteheads.s0laFunk",
        "noteheads.d0tiFunk",
        "noteheads.u0tiFunk",

        "noteheads.d1doFunk",
        "noteheads.u1doFunk",
        "noteheads.d1reFunk",
        "noteheads.u1reFunk",
        "noteheads.d1miFunk",
        "noteheads.u1miFunk",
        "noteheads.d1faFunk",
        "noteheads.u1faFunk",
        "noteheads.s1solFunk",
        "noteheads.s1laFunk",
        "noteheads.d1tiFunk",
        "noteheads.u1tiFunk",

        "noteheads.d2doFunk",
        "noteheads.u2doFunk",
        "noteheads.d2reFunk",
        "noteheads.u2reFunk",
        "noteheads.s2miFunk",
        "noteheads.d2faFunk",
        "noteheads.u2faFunk",
        "noteheads.s2solFunk",
        "noteheads.s2laFunk",
        "noteheads.d2tiFunk",
        "noteheads.u2tiFunk",

        "noteheads.s0doWalker",
        "noteheads.s0reWalker",
        "noteheads.s0miWalker",
        "noteheads.s0faWalker",
        "noteheads.s0laWalker",
        "noteheads.s0tiWalker",

        "noteheads.d1doWalker",
        "noteheads.u1doWalker",
        "noteheads.d1reWalker",
        "noteheads.u1reWalker",
        "noteheads.s1miWalker",
        "noteheads.d1faWalker",
        "noteheads.u1faWalker",
        "noteheads.s1laWalker",
        "noteheads.d1tiWalker",
        "noteheads.u1tiWalker",

        "noteheads.d2doWalker",
        "noteheads.u2doWalker",
        "noteheads.d2reWalker",
        "noteheads.u2reWalker",
        "noteheads.s2miWalker",
        "noteheads.d2faWalker",
        "noteheads.u2faWalker",
        "noteheads.s2laWalker",
        "noteheads.d2tiWalker",
        "noteheads.u2tiWalker",

        "noteheads.s0doThin",
        "noteheads.s0reThin",
        "noteheads.s0miThin",
        "noteheads.d0faThin",
        "noteheads.u0faThin",
        "noteheads.s0laThin",
        "noteheads.s0tiThin",

        "noteheads.s1doThin",
        "noteheads.s1reThin",
        "noteheads.s1miThin",
        "noteheads.d1faThin",
        "noteheads.u1faThin",
        "noteheads.s1laThin",
        "noteheads.s1tiThin",

        "noteheads.s2doThin",
        "noteheads.s2reThin",
        "noteheads.s2miThin",
        "noteheads.d2faThin",
        "noteheads.u2faThin",
        "noteheads.s2laThin",
        "noteheads.s2tiThin",
    ]),

    LilypondGlyphset("S", [
        # modern clefs
        "clefs.G",
        "clefs.G_change",
        "clefs.GG",
        "clefs.GG_change",
        "clefs.tenorG",
        "clefs.tenorG_change",
        "clefs.F",
        "clefs.F_change",
        "clefs.C",
        "clefs.C_change",
        "clefs.varC",
        "clefs.varC_change",

        "clefs.percussion",
        "clefs.percussion_change",
        "clefs.varpercussion",
        "clefs.varpercussion_change",

        "clefs.tab",
        "clefs.tab_change",

        # ancient clefs
        "clefs.mensural.c",
        "clefs.mensural.c_change",
        "clefs.mensural.f",
        "clefs.mensural.f_change",
        "clefs.mensural.g",
        "clefs.mensural.g_change",

        "clefs.neomensural.c",
        "clefs.neomensural.c_change",

        "clefs.blackmensural.c",
        "clefs.blackmensural.c_change",

        "clefs.petrucci.c1",
        "clefs.petrucci.c1_change",
        "clefs.petrucci.c2",
        "clefs.petrucci.c2_change",
        "clefs.petrucci.c3",
        "clefs.petrucci.c3_change",
        "clefs.petrucci.c4",
        "clefs.petrucci.c4_change",
        "clefs.petrucci.c5",
        "clefs.petrucci.c5_change",
        "clefs.petrucci.f",
        "clefs.petrucci.f_change",
        "clefs.petrucci.g",
        "clefs.petrucci.g_change",

        "clefs.kievan.do",
        "clefs.kievan.do_change",

        # clefs for Gregorian chant
        "clefs.vaticana.do",
        "clefs.vaticana.do_change",
        "clefs.vaticana.fa",
        "clefs.vaticana.fa_change",

        "clefs.hufnagel.do",
        "clefs.hufnagel.do_change",
        "clefs.hufnagel.do.fa",
        "clefs.hufnagel.do.fa_change",
        "clefs.hufnagel.fa",
        "clefs.hufnagel.fa_change",

        "clefs.medicaea.do",
        "clefs.medicaea.do_change",
        "clefs.medicaea.fa",
        "clefs.medicaea.fa_change",

        # various modern musical symbols
        "scripts.tenuto",
        "scripts.sforzato",
        "scripts.dmarcato",
        "scripts.umarcato",
        "scripts.dportato",
        "scripts.uportato",
        "scripts.snappizzicato",
        "scripts.staccato",
        "scripts.dstaccatissimo",
        "scripts.ustaccatissimo",
        "scripts.espr",

        "scripts.flageolet",
        "scripts.open",
        "scripts.halfopen",
        "scripts.halfopenvertical",
        "scripts.stopped",
        "scripts.thumb",

        "scripts.dfermata",
        "scripts.ufermata",
        "scripts.dshortfermata",
        "scripts.ushortfermata",
        "scripts.dveryshortfermata",
        "scripts.uveryshortfermata",
        "scripts.dhenzeshortfermata",
        "scripts.uhenzeshortfermata",
        "scripts.dlongfermata",
        "scripts.ulongfermata",
        "scripts.dverylongfermata",
        "scripts.uverylongfermata",
        "scripts.dhenzelongfermata",
        "scripts.uhenzelongfermata",

        "scripts.coda",
        "scripts.varcoda",
        "scripts.segno",
        "scripts.varsegno",

        "scripts.caesura.curved",
        "scripts.caesura.straight",
        "scripts.lcomma",
        "scripts.rcomma",
        "scripts.lvarcomma",
        "scripts.rvarcomma",
        "scripts.laltcomma",
        "scripts.raltcomma",
        "scripts.tickmark",

        "scripts.trill",
        "scripts.trill_element",
        "scripts.prall",
        "scripts.pralldown",
        "scripts.prallup",
        "scripts.mordent",
        "scripts.downmordent",
        "scripts.upmordent",
        "scripts.downprall",
        "scripts.upprall",
        "scripts.prallmordent",
        "scripts.prallprall",
        "scripts.lineprall",
        "scripts.turn",
        "scripts.reverseturn",
        "scripts.slashturn",
        "scripts.haydnturn",

        "scripts.downbow",
        "scripts.upbow",
        "scripts.dpedaltoe",
        "scripts.upedaltoe",
        "scripts.dpedalheel",
        "scripts.upedalheel",

        "scripts.arpeggio",
        "scripts.arpeggio.arrow.1",
        "scripts.arpeggio.arrow.M1",

        # ancient musical symbols
        "scripts.barline.kievan",

        "scripts.circulus",
        "scripts.dsemicirculus",
        "scripts.usemicirculus",
        "scripts.daccentus",
        "scripts.uaccentus",
        "scripts.ictus",
        "scripts.dsignumcongruentiae",
        "scripts.usignumcongruentiae",
    ]),

    LilypondGlyphset("O", [
        # modern accidentals
        "accidentals.flat",
        "accidentals.flat.slash",
        "accidentals.flat.arrowdown",
        "accidentals.flat.arrowup",
        "accidentals.flat.arrowboth",
        "accidentals.flat.slashslash",
        "accidentals.flatflat",
        "accidentals.flatflat.slash",
        "accidentals.mirroredflat",
        "accidentals.mirroredflat.backslash",
        "accidentals.mirroredflat.flat",

        "accidentals.sharp",
        "accidentals.sharp.arrowdown",
        "accidentals.sharp.arrowup",
        "accidentals.sharp.arrowboth",
        "accidentals.sharp.slash.stem",
        "accidentals.sharp.slashslash.stem",
        "accidentals.sharp.slashslash.stemstemstem",
        "accidentals.sharp.slashslashslash.stem",
        "accidentals.sharp.slashslashslash.stemstem",
        "accidentals.doublesharp",

        "accidentals.natural",
        "accidentals.natural.arrowdown",
        "accidentals.natural.arrowup",
        "accidentals.natural.arrowboth",

        "accidentals.leftparen",
        "accidentals.rightparen",

        # accidentals for Persian music
        "accidentals.flat.koron",
        "accidentals.sharp.sori",

        # ancient accidentals
        "accidentals.mensural1",
        "accidentals.mensuralM1",
        "accidentals.vaticana0",
        "accidentals.vaticanaM1",
        "accidentals.hufnagelM1",
        "accidentals.medicaeaM1",
        "accidentals.kievan1",
        "accidentals.kievanM1",

        # modern time signatures
        "timesig.C22",
        "timesig.C44",

        # ancient time signatures
        "timesig.mensural22",
        "timesig.mensural24",
        "timesig.mensural32",
        "timesig.mensural34",
        "timesig.mensural44",
        "timesig.mensural48",
        "timesig.mensural64",
        "timesig.mensural68",
        "timesig.mensural68alt",
        "timesig.mensural94",
        "timesig.mensural98",

        "timesig.neomensural22",
        "timesig.neomensural24",
        "timesig.neomensural32",
        "timesig.neomensural34",
        "timesig.neomensural44",
        "timesig.neomensural48",
        "timesig.neomensural64",
        "timesig.neomensural68",
        "timesig.neomensural68alt",
        "timesig.neomensural94",
        "timesig.neomensural98",

        # modern rests
        "rests.0",
        "rests.0o",
        "rests.1",
        "rests.1o",
        "rests.2",
        "rests.2classical",
        "rests.2z",
        "rests.3",
        "rests.4",
        "rests.5",
        "rests.6",
        "rests.7",
        "rests.8",
        "rests.9",
        "rests.10",
        "rests.M1",
        "rests.M1o",
        "rests.M2",
        "rests.M3",

        # ancient rests
        "rests.0mensural",
        "rests.1mensural",
        "rests.2mensural",
        "rests.3mensural",
        "rests.4mensural",
        "rests.M1mensural",
        "rests.M2mensural",
        "rests.M3mensural",

        "rests.0neomensural",
        "rests.1neomensural",
        "rests.2neomensural",
        "rests.3neomensural",
        "rests.4neomensural",
        "rests.M1neomensural",
        "rests.M2neomensural",
        "rests.M3neomensural",

        # augmentation dots (for notes and rests)
        "dots.dot",
        "dots.dotvaticana",
        "dots.dotkievan",

        # modern flags
        "flags.d3",
        "flags.u3",
        "flags.d4",
        "flags.u4",
        "flags.d5",
        "flags.u5",
        "flags.d6",
        "flags.u6",
        "flags.d7",
        "flags.u7",
        "flags.d8",
        "flags.u8",
        "flags.d9",
        "flags.u9",
        "flags.d10",
        "flags.u10",

        "flags.dgrace",
        "flags.ugrace",

        # ancient flags
        "flags.mensurald03",
        "flags.mensuralu03",
        "flags.mensurald04",
        "flags.mensuralu04",
        "flags.mensurald05",
        "flags.mensuralu05",
        "flags.mensurald06",
        "flags.mensuralu06",

        "flags.mensurald13",
        "flags.mensuralu13",
        "flags.mensurald14",
        "flags.mensuralu14",
        "flags.mensurald15",
        "flags.mensuralu15",
        "flags.mensurald16",
        "flags.mensuralu16",

        "flags.mensurald23",
        "flags.mensuralu23",
        "flags.mensurald24",
        "flags.mensuralu24",
        "flags.mensurald25",
        "flags.mensuralu25",
        "flags.mensurald26",
        "flags.mensuralu26",

        # pedal symbol elements
        "pedal.P",
        "pedal.e",
        "pedal.d",
        "pedal..",
        "pedal.Ped",
        "pedal.*",
        "pedal.M",

        # accordion symbols
        "accordion.bayanbass",
        "accordion.discant",
        "accordion.dot",
        "accordion.freebass",
        "accordion.oldEE",
        "accordion.pull",
        "accordion.push",
        "accordion.stdbass",

        # arrow heads (mainly for combination with other glyphs)
        "arrowheads.open.01",
        "arrowheads.close.01",
        "arrowheads.open.0M1",
        "arrowheads.close.0M1",
        "arrowheads.open.11",
        "arrowheads.close.11",
        "arrowheads.open.1M1",
        "arrowheads.close.1M1",

        # miscellaneous symbols
        "brackettips.down",
        "brackettips.up",
        "ties.lyric.default",
        "ties.lyric.short",

        # ancient custodes
        "custodes.mensural.d0",
        "custodes.mensural.u0",
        "custodes.mensural.d1",
        "custodes.mensural.u1",
        "custodes.mensural.d2",
        "custodes.mensural.u2",

        "custodes.vaticana.d0",
        "custodes.vaticana.u0",
        "custodes.vaticana.d1",
        "custodes.vaticana.u1",
        "custodes.vaticana.d2",
        "custodes.vaticana.u2",

        "custodes.hufnagel.d0",
        "custodes.hufnagel.u0",
        "custodes.hufnagel.d1",
        "custodes.hufnagel.u1",
        "custodes.hufnagel.d2",
        "custodes.hufnagel.u2",

        "custodes.medicaea.d0",
        "custodes.medicaea.u0",
        "custodes.medicaea.d1",
        "custodes.medicaea.u1",
        "custodes.medicaea.d2",
        "custodes.medicaea.u2",
    ]),

    # All Emmentaler glyphs that can be used in 'text mode' and have a
    # real Unicode 'cmap' mapping (i.e., not only a code point from
    # the PUA) should be part of this array.  The corresponding
    # Emmentaler encoding is the only one used for communication with
    # the Pango library.
    #
    # Glyphs that are results of input ligatures like '4+' should
    # appear here, too.
    LilypondGlyphset("P", [
        ##
        ## text-mode glyphs
        ##

        # letters
        "space",
        "f",
        "m",
        "n",
        "p",
        "r",
        "s",
        "z",

        # punctuation
        "comma",
        "hyphen",
        "period",
        "plus",

        # digits
        "zero",
        "one",
        "two",
        "three",
        "four",
        "four.alt",
        "five",
        "six",
        "seven",
        "seven.alt",
        "eight",
        "nine",

        # fixed-width digits
        "fixedwidth.zero",
        "fixedwidth.one",
        "fixedwidth.two",
        "fixedwidth.three",
        "fixedwidth.four",
        "fixedwidth.four.alt",
        "fixedwidth.five",
        "fixedwidth.six",
        "fixedwidth.seven",
        "fixedwidth.seven.alt",
        "fixedwidth.eight",
        "fixedwidth.nine",

        # fattened digits
        "fattened.zero",
        "fattened.one",
        "fattened.two",
        "fattened.three",
        "fattened.four",
        "fattened.four.alt",
        "fattened.five",
        "fattened.six",
        "fattened.seven",
        "fattened.seven.alt",
        "fattened.eight",
        "fattened.nine",

        # fattened, fixed-width digits
        "fattened.fixedwidth.zero",
        "fattened.fixedwidth.one",
        "fattened.fixedwidth.two",
        "fattened.fixedwidth.three",
        "fattened.fixedwidth.four",
        "fattened.fixedwidth.four.alt",
        "fattened.fixedwidth.five",
        "fattened.fixedwidth.six",
        "fattened.fixedwidth.seven",
        "fattened.fixedwidth.seven.alt",
        "fattened.fixedwidth.eight",
        "fattened.fixedwidth.nine",

        ##
        ## other symbols
        ##

        # accidentals for figured bass
        "accidentals.flat.figbass",
        "accidentals.flatflat.figbass",
        "accidentals.sharp.figbass",
        "accidentals.doublesharp.figbass",
        "accidentals.natural.figbass",

        # symbols for figured bass
        "figbass.twoplus",
        "figbass.fourplus",
        "figbass.fiveplus",
        "figbass.sixstroked",
        "figbass.sevenstroked",
        "figbass.ninestroked",
    ]),
]


# Glyphs for braces are called 'brace0', 'brace1', ..., 'brace575'.
# We split them into four glyph sets, using the same suffixes as the
# Emmentaler encodings.

def make_brace(start, stop):
    return [f"brace{x}" for x in range(start, stop)]

braces_glyphsets = [
    LilypondGlyphset("N", make_brace(0, 200)),
    LilypondGlyphset("S", make_brace(200, 400)),
    LilypondGlyphset("O", make_brace(400, 500)),
    LilypondGlyphset("P", make_brace(500, 576)),
]


# Ensure that all encodings have no more than 255 elements (one slot
# each must be reserved for the `/.notdef` glyph).
for e in emmentaler_glyphsets:
    if len(e.glyphset) > 255:
        raise SystemExit(
            f"'{e.suffix}' Emmentaler encoding has more than 255 elements!")

for b in braces_glyphsets:
    if len(b.glyphset) > 255:
        raise SystemExit(
            f"'{b.suffix}' Emmentaler brace encoding has more than 255 elements!")


def emit_encoding(encoding, prefix):
    print()
    print(f"/{prefix}{encoding.suffix}Encoding [")
    print("\t/.notdef")
    for s in sorted(encoding.glyphset):
        print(f"\t/{s}")
    print("] def")


def emit_glyph_commands(encoding):
    print()
    for i, s in enumerate(sorted(encoding.glyphset), start=1):
        print(f"/{s} {{<{i:02x}> show}} def")


def emit_font_directory_entry(encodings, prefix, size):
    print()
    print(f"FontDirectory /Emmentaler-{size} known {{")
    for es in encodings:
        print(f"\t/Emmentaler-{size} findfont dup length dict copy begin")
        print(f"\t/Encoding {prefix}{es.suffix}Encoding def")
        print(f"\t/Emmentaler-{size}-{es.suffix} currentdict definefont pop end")
    print(f"}} if")


def emit_scheme_hash(encodings, prefix, func):
    num_elem = 0
    for es in encodings:
        num_elem += len(es.glyphset)

    print()
    print(f"(define-public {prefix}-encoding-table (make-hash-table {num_elem}))")
    print(f"(define ({func} k v) (hash-set! {prefix}-encoding-table k v))")

    for es in encodings:
        print()
        for s in sorted(es.glyphset):
            print(f'({func} "{s}" "{es.suffix}")')


postscript_header = """\
%!PS-Adobe-2.0: encodingdefs.ps
%
% Custom encodings and definitions for `Emmentaler-XX` and the
% `Emmentaler-Brace` fonts.
%
% This file was generated by `generate-encodings.py`.  It is part of
% LilyPond, the GNU music typesetter.
%
% Copyright (C) 2015--2022 Knut Petersen <knut_petersen@t-online.de>,
%               2018--2019 Malte Meyn <lilypond@maltemeyn.de>
%               2022 Werner Lemberg <wl@gnu.org>
%
% LilyPond is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
%
% LilyPond is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.\
"""

scm_header="""\
;;;; font-encodings.scm
;;;;
;;;; A hash to quickly retrieve the corresponding font encoding of a glyph
;;;; name.
;;;;
;;;; This file was generated by `generate-encodings.ly`.  It is part of
;;;; LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2022 Werner Lemberg <wl@gnu.org>
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
;;;; along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.\
"""


p = argparse.ArgumentParser(description="Generate font encoding stuff.")
p.add_argument("-s", help="create Scheme code", action="store_true")
args = p.parse_args()


if args.s:
    print(scm_header)

    emit_scheme_hash(emmentaler_glyphsets, "glyph", "g")
    emit_scheme_hash(braces_glyphsets, "brace", "b")
else:
    print(postscript_header)

    for e in emmentaler_glyphsets:
        emit_encoding(e, "Lily")
        emit_glyph_commands(e)

    for b in braces_glyphsets:
        emit_encoding(b, "Brace")
        emit_glyph_commands(b)

    emit_font_directory_entry(braces_glyphsets, "Brace", "Brace")

    for s in [11, 13, 14, 16, 18, 20, 23, 26]:
        emit_font_directory_entry(emmentaler_glyphsets, "Lily", s)

# eof
