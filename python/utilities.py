# utilities.py
# -*- coding: utf-8 -*-
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2016--2023 John Gourlay <john@weathervanefarm.net>
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


import re


def escape_ly_output_string(input_string):
    return_string = input_string
    needs_quotes = not re.match("^[a-zA-ZäöüÜÄÖßñ]+$", return_string)
    if needs_quotes:
        return_string = ('"'
                         + return_string.replace('\\', '\\\\')
                                        .replace('"', r'\"')
                         + '"')
    return return_string


def interpret_alter_element(alter_elm):
    alter = 0
    if alter_elm:
        val = eval(alter_elm.get_text())
        if type(val) in (int, float):
            alter = val
    return alter


def musicxml_duration_to_log(dur):
    return {'1024th': 10,
            '512th': 9,
            '256th': 8,
            '128th': 7,
            '64th': 6,
            '32nd': 5,
            '16th': 4,
            'eighth': 3,
            'quarter': 2,
            'half': 1,
            'whole': 0,
            'breve': -1,
            'longa': -2,
            'long': -2}.get(dur, 0)


# Examples:
#
#   r'"foo  \"bar  baz\"  urgh"'  ->  [r'"foo  \"bar  baz\"  urgh"']
#   r'foo  "bar  baz"  urgh'      ->  ['foo', '"bar  baz"', 'urgh']
#
# We only split at ASCII whitespace characters, mainly to preserve the
# non-breakable space character (U+0080).
def split_string_and_preserve_doublequoted_substrings(value):
    return re.findall(r'(?asx) (?: " .*? [^\\] " | \S )+', value)


def musicxml_sound_to_lilypond_midi_instrument(sound):
    sounds = {
        "brass.french-horn": 'french horn',
        "brass.group": 'brass section',
        "brass.group.synth": 'synthbrass 1',
        "brass.trombone": 'trombone',
        "brass.trombone.alto": 'trombone',
        "brass.trombone.bass": 'trombone',
        "brass.trombone.contrabass": 'trombone',
        "brass.trombone.tenor": 'trombone',
        "brass.trumpet": 'trumpet',
        "brass.trumpet.baroque": 'trumpet',
        "brass.trumpet.bass": 'trumpet',
        "brass.trumpet.bflat": 'trumpet',
        "brass.trumpet.c": 'trumpet',
        "brass.trumpet.d": 'trumpet',
        "brass.trumpet.piccolo": 'trumpet',
        "brass.trumpet.pocket": 'trumpet',
        "brass.trumpet.slide": 'trumpet',
        "brass.trumpet.tenor": 'trumpet',
        "brass.tuba": 'tuba',
        "brass.tuba.bass": 'tuba',
        "brass.tuba.subcontrabass": 'tuba',
        "brass.wagner-tuba": 'french horn',
        "drum.timpani": 'timpani',
        "drum.tom-tom": 'melodic tom',
        "drum.tom-tom.synth": 'synth drum',
        "effect.applause": 'applause',
        "effect.bass-string-slap": 'slap bass 1',
        "effect.bird": 'bird tweet',
        "effect.bird.tweet": 'bird tweet',
        "effect.breath": 'breath noise',
        "effect.guitar-fret": 'guitar fret noise',
        "effect.gunshot": 'gunshot',
        "effect.helicopter": 'helicopter',
        "effect.metronome-click": 'woodblock',
        "effect.rain": 'fx1 (rain)',
        "effect.seashore": 'seashore',
        "effect.telephone-ring": 'telephone ring',
        "keyboard.accordion": 'accordion',
        "keyboard.bandoneon": 'accordion',
        "keyboard.celesta": 'celesta',
        "keyboard.clavichord": 'clav',
        "keyboard.concertina": 'concertina',
        "keyboard.harpsichord": 'harpsichord',
        "keyboard.ondes-martenot": 'ocarina',
        "keyboard.organ": 'church organ',
        "keyboard.organ.drawbar": 'drawbar organ',
        "keyboard.organ.percussive": 'percussive organ',
        "keyboard.organ.pipe": 'church organ',
        "keyboard.organ.reed": 'reed organ',
        "keyboard.piano": 'acoustic grand',
        "keyboard.piano.electric": 'electric piano 1',
        "keyboard.piano.grand": 'acoustic grand',
        "keyboard.piano.honky-tonk": 'honky-tonk',
        "metal.bells.agogo": 'agogo',
        "metal.bells.tinklebell": 'tinkle bell',
        "metal.cymbal.reverse": 'reverse cymbal',
        "pitched-percussion.glockenspiel": 'glockenspiel',
        "pitched-percussion.glockenspiel.alto": 'glockenspiel',
        "pitched-percussion.glockenspiel.soprano": 'glockenspiel',
        "pitched-percussion.hammer-dulcimer": 'dulcimer',
        "pitched-percussion.kalimba": 'kalimba',
        "pitched-percussion.marimba": 'marimba',
        "pitched-percussion.marimba.bass": 'marimba',
        "pitched-percussion.music-box": 'music box',
        "pitched-percussion.tubular-bells": 'tubular bells',
        "pitched-percussion.vibraphone": 'vibraphone',
        "pitched-percussion.xylophone": 'xylophone',
        "pitched-percussion.xylophone.alto": 'xylophone',
        "pitched-percussion.xylophone.bass": 'xylophone',
        "pitched-percussion.xylophone.soprano": 'xylophone',
        "pitched-percussion.xylorimba": 'xylophone',
        "pluck.banjo": 'banjo',
        "pluck.banjo.tenor": 'banjo',
        "pluck.bass": 'acoustic bass',
        "pluck.bass.acoustic": 'acoustic bass',
        "pluck.bass.electric": 'electric bass',
        "pluck.bass.fretless": 'fretless bass',
        "pluck.bass.synth": 'synth bass 1',
        "pluck.dulcimer": 'dulcimer',
        "pluck.guitar": 'acoustic guitar (nylon)',
        "pluck.guitar.acoustic": 'acoustic guitar (nylon)',
        "pluck.guitar.electric": 'electric guitar (jazz)',
        "pluck.guitar.nylon-string": 'acoustic guitar (nylon)',
        "pluck.guitar.steel-string": 'acoustic guitar (steel)',
        "pluck.harp": 'orchestral harp',
        "pluck.lute": 'acoustic guitar (nylon)',
        "pluck.shamisen": 'shamisen',
        "pluck.sitar": 'sitar',
        "strings.cello": 'cello',
        "strings.cello.piccolo": 'cello',
        "strings.contrabass": 'contrabass',
        "strings.fiddle": 'fiddle',
        "strings.group.synth": 'synth strings 1',
        "strings.viola": 'viola',
        "strings.violin": 'violin',
        "synth.effects.atmosphere": 'fx 4 (atmosphere)',
        "synth.effects.brightness": 'fx 5 (brightness)',
        "synth.effects.crystal": 'fx 3 (crystal)',
        "synth.effects.echoes": 'fx 7 echoes',
        "synth.effects.goblins": 'fx 6 goblins',
        "synth.effects.rain": 'fx 1 rain',
        "synth.effects.sci-fi": 'fx 8 sci-fi',
        "synth.effects.soundtrack": 'fx 2 (soundtrack)',
        "synth.pad.bowed": 'pad 5 bowed',
        "synth.pad.choir": 'pad 4 choir',
        "synth.pad.halo": 'pad 7 halo',
        "synth.pad.metallic": 'pad 6 metallic',
        "synth.pad.polysynth": 'pad 3 polysynth',
        "synth.pad.sweep": 'pad 8 sweep',
        "synth.pad.warm": 'pad 2 warm',
        "synth.tone.sawtooth": 'lead 1 (square)',
        "synth.tone.square": 'lead 2 (sawtooth)',
        "voice.aa": 'choir aahs',
        "voice.alto": 'choir aahs',
        "voice.aw": 'choir aahs',
        "voice.baritone": 'choir aahs',
        "voice.bass": 'choir aahs',
        "voice.child": 'choir aahs',
        "voice.countertenor": 'choir aahs',
        "voice.doo": 'choir aahs',
        "voice.ee": 'choir aahs',
        "voice.female": 'choir aahs',
        "voice.kazoo": 'choir aahs',
        "voice.male": 'choir aahs',
        "voice.mezzo-soprano": 'choir aahs',
        "voice.mm": 'choir aahs',
        "voice.oo": 'voice oohs',
        "voice.soprano": 'choir aahs',
        "voice.synth": 'synth voice',
        "wind.flutes.blown-bottle": 'blown bottle',
        "wind.flutes.calliope": 'lead 3 (calliope)',
        "wind.flutes.flute": 'flute',
        "wind.flutes.flute.alto": 'flute',
        "wind.flutes.flute.bass": 'flute',
        "wind.flutes.flute.contra-alto": 'flute',
        "wind.flutes.flute.contrabass": 'flute',
        "wind.flutes.flute.double-contrabass": 'flute',
        "wind.flutes.flute.piccolo": 'flute',
        "wind.flutes.flute.subcontrabass": 'flute',
        "wind.flutes.ocarina": 'ocarina',
        "wind.flutes.recorder": 'recorder',
        "wind.flutes.recorder.alto": 'recorder',
        "wind.flutes.recorder.bass": 'recorder',
        "wind.flutes.recorder.contrabass": 'recorder',
        "wind.flutes.recorder.descant": 'recorder',
        "wind.flutes.recorder.garklein": 'recorder',
        "wind.flutes.recorder.great-bass": 'recorder',
        "wind.flutes.recorder.sopranino": 'recorder',
        "wind.flutes.recorder.soprano": 'recorder',
        "wind.flutes.recorder.tenor": 'recorder',
        "wind.flutes.shakuhachi": 'shakuhachi',
        "wind.flutes.whistle": 'whistle',
        "wind.flutes.whistle.alto": 'whistle',
        "wind.pipes.bagpipes": 'bagpipe',
        "wind.reed.basset-horn": 'clarinet',
        "wind.reed.bassoon": 'bassoon',
        "wind.reed.clarinet": 'clarinet',
        "wind.reed.clarinet.a": 'clarinet',
        "wind.reed.clarinet.alto": 'clarinet',
        "wind.reed.clarinet.bass": 'clarinet',
        "wind.reed.clarinet.basset": 'clarinet',
        "wind.reed.clarinet.bflat": 'clarinet',
        "wind.reed.clarinet.contra-alto": 'clarinet',
        "wind.reed.clarinet.contrabass": 'clarinet',
        "wind.reed.clarinet.eflat": 'clarinet',
        "wind.reed.clarinet.piccolo.aflat": 'clarinet',
        "wind.reed.contrabass": 'contrabass',
        "wind.reed.contrabassoon": 'bassoon',
        "wind.reed.english-horn": 'oboe',
        "wind.reed.harmonica": 'harmonica',
        "wind.reed.harmonica.bass": 'harmonica',
        "wind.reed.oboe": 'oboe',
        "wind.reed.oboe.bass": 'oboe',
        "wind.reed.oboe.piccolo": 'oboe',
        "wind.reed.oboe-da-caccia": 'oboe',
        "wind.reed.oboe-damore": 'oboe',
        "wind.reed.saxophone": 'alto sax',
        "wind.reed.saxophone.alto": 'alto sax',
        "wind.reed.saxophone.baritone": 'baritone sax',
        "wind.reed.saxophone.bass": 'baritone sax',
        "wind.reed.saxophone.contrabass": 'baritone sax',
        "wind.reed.saxophone.melody": 'soprano sax',
        "wind.reed.saxophone.mezzo-soprano": 'soprano sax',
        "wind.reed.saxophone.sopranino": 'soprano sax',
        "wind.reed.saxophone.sopranissimo": 'soprano sax',
        "wind.reed.saxophone.soprano": 'soprano, sax',
        "wind.reed.saxophone.subcontrabass": 'baritone sax',
        "wind.reed.saxophone.tenor": 'tenor sax',
        "wind.reed.shenai": 'shanai',
        "wood.temple-block": 'wood block',
        "wood.wood-block": 'wood block',
    }
    return sounds.get(sound, 'acoustic grand')
