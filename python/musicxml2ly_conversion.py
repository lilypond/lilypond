# -*- coding: utf-8 -*-
from rational import *
import lilylib as ly
import musicexp
_ = ly._

def rational_to_lily_duration(rational_len):
    d = musicexp.Duration()

    rational_len.normalize_self()
    d_log = {1: 0, 2: 1, 4:2, 8:3, 16:4, 32:5, 64:6, 128:7, 256:8, 512:9}.get(rational_len.denominator(), -1)

    # Duration of the form 1/2^n or 3/2^n can be converted to a simple lilypond duration
    dots = {1: 0, 3: 1, 7: 2, 15: 3, 31: 4, 63: 5, 127: 6}.get(rational_len.numerator(), -1)
    if(d_log >= dots >= 0):
        # account for the dots!
        d.duration_log = d_log - dots
        d.dots = dots
    elif(d_log >= 0):
        d.duration_log = d_log
        d.factor = Rational(rational_len.numerator())
    else:
        ly.warning(_("Encountered rational duration with denominator %s, "
                       "unable to convert to lilypond duration") %
                    rational_len.denominator())
        # TODO: Test the above error message
        return None

    return d

def musicxml_step_to_lily(step):
    if step:
        return (ord(step) - ord('A') + 7 - 2) % 7
    else:
        return None

class Marker(musicexp.Music):
    def __init__(self):
        self.direction = 0
        self.event = None
    def print_ly(self, printer):
        ly.warning(_("Encountered unprocessed marker %s\n") % self)
        pass
    def ly_expression(self):
        return ""
    
class RepeatMarker(Marker):
    def __init__(self):
        Marker.__init__(self)
        self.times = 0

class EndingMarker(Marker):
    pass
