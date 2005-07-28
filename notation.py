import string
import re
import gnomecanvas
import gtk
import os
import socket
import music
import pango
import math
from rational import Rational

display_dpi = 75

def get_display_dpi():
    str = os.popen ('xdpyinfo | grep "dots per inch"').read ()
    m = re.match ('([0-9]+)x([0-9]+) dots')
    if m:
        display_dpi = (string.atoi (m.group (2)) + string.atoi (m.group (2)))/2


scale_alterations = [0, 0, -2, 0, 0,-2,-2]  

copy_lilypond_input = 1
time_sig = (4, 4)
measure_length = Rational (time_sig[0], time_sig[1])
measure_count = 4

scale_str = ("'(%s)" % 
             string.join (['(%d . %d)' % (i , scale_alterations[i]) for i in range (0,7)], ' '))

"'((0 . 0) (1 . 0) (2 . -2) (3 . 0) (4 . 0) (5 . -2) (6 . -2))"
clefsetting = """
      (context-spec-music
       (make-property-set 'clefGlyph "clefs.C") 'Staff)
      (context-spec-music
       (make-property-set 'clefPosition 0) 'Staff)
      (context-spec-music
       (make-property-set 'middleCPosition 0) 'Staff)
"""

try:
    server = os.environ['IKEBANASERVER']
except KeyError:
    server = 'localhost'

lilypond_input_log_file = open ("input.log", 'w')

def talk_to_lilypond (expression_str):
    """Send a LISP expression to LilyPond, wait for return value."""
    if copy_lilypond_input:
        lilypond_input_log_file.write (expression_str)
        lilypond_input_log_file.flush ()
    
    sock = socket.socket (socket.AF_INET)
    address = (server, 2904)
    sock.connect (address)
    sock.send (expression_str, socket.MSG_WAITALL)

    cont = 1
    retval = ''
    while  cont:
	try:
	    (data, from_addr) = sock.recvfrom (1024)
	except socket.error:
	    break
	cont = len (data) > 0
	retval += data
 
    return retval

def set_measure_number (str, num):
    return """(make-music 'SequentialMusic 'elements (list
      (context-spec-music
       (make-property-set 'timeSignatureFraction (cons %d %d)) 'Score)
      (context-spec-music
       (make-property-set 'measureLength (ly:make-moment %d %d)) 'Score)
      (context-spec-music
       (make-property-set 'beatLength (ly:make-moment 1 %d)) 'Score)
      (context-spec-music
       (make-property-set 'currentBarNumber %d) 'Score)
      (context-spec-music
       (make-music 'EventChord
        'elements
        (list
         (make-music 'KeyChangeEvent
          'pitch-alist
          %s)
         ))
       'Staff)

    %s))""" % (time_sig[0], time_sig[1], time_sig[0],
               time_sig[1], time_sig[1], num, scale_str, str)

def render_score (filename, ly):
    print ly
    str =  '''
myNotes = %s
\\score  { \myNotes }
''' % ly
    open (filename, 'w').write (str)
    base = os.path.splitext (filename)[0] + '.ps'
    os.system ('(lilypond %s && gv %s)&  ' % (filename, base))

def add_start_skip (str, start_skip):
    return """(make-music 'SequentialMusic 'elements
               (list
                (make-music 'SkipMusic
                            'duration (ly:make-duration 0 0 %d %d))
                %s))
""" % (start_skip.num, start_skip.den, str)
    
    

class Lilypond_socket_parser:
    """Maintain state of reading multi-line lilypond output for socket transport."""
    def __init__ (self, interpret_function):
        self.clear ()
        self.interpret_socket_line = interpret_function

    def clear (self):
        self.cause_tag = None
	self.bbox = None
        
    def parse_line (self, line):
        fields = string.split (line)
        if not fields:
            return
        offset = (0,0)
	if 0:
	    pass
	elif fields[0] == 'hello':
	    print 'found server: ', fields[1:]
	    return
        elif fields[0] == 'at':
                offset = (string.atof (fields[1]),
                          string.atof (fields[2]))
                fields = fields[3:]
        elif fields[0] == 'nocause':
            self.cause_tag = None
	    self.bbox = None
            return
        elif fields[0] == 'cause':
            self.cause_tag = string.atoi (fields[1])
            self.name = fields[2][1:-1]
	    self.bbox = tuple (map (string.atof, fields[3:]))
            return

        return self.interpret_socket_line (offset, self.cause_tag,
					   self.bbox, self.name,
                                           fields)

class Notation_controller:
    """Couple Notation and the music model. Stub for now. """
    def __init__ (self, music_document):
	self.document = music_document
        
        self.notation = Notation (self)
        self.parser = Lilypond_socket_parser (self.interpret_line)
        self.start_moment = Rational (0)
        self.stop_moment = Rational (3)

    def interpret_line (self, offset, cause, bbox, name, fields):
	notation_item = self.notation.add_item (offset, cause, bbox, fields)
        notation_item.name = name
    def touch_document (self):
        self.document.touched = True
        
    def update_notation(self):
	doc = self.document
	doc.recompute()
	
	expr = doc.music

       	def sub(x):
		ok = (x.start >= self.start_moment and
                      x.start + x.length() <= self.stop_moment)
		return ok

        def sub2 (x):
            return x.name() in ('RestEvent','NoteEvent') and  sub(x)

        start_note = expr.find_first (sub2)

        start_skip = start_note.start - start_note.start.floor()
        
	str = expr.lisp_sub_expression (sub)
        str = add_start_skip (str, start_skip)
        
        bar_count = (self.start_moment / measure_length).floor()
        str = set_measure_number (str, bar_count.num)
	str = talk_to_lilypond (str)
        self.parse_socket_file (str)

    def ensure_visible (self, when):
        new_start =  max ((when - measure_length).floor(), Rational(0))
        new_stop = new_start + Rational (measure_count) * measure_length

        if new_start <> self.start_moment or new_stop <> self.stop_moment:
            print "render interval", new_start, new_stop
            self.touch_document()
            
        self.start_moment = new_start
        self.stop_moment = new_stop

    def parse_socket_file (self, str):
        self.notation.clear ()
        lines = string.split (str, '\n')
        self.parse_lines (lines)
        self.notation.touched = True
        self.document.touched = False
        
    def parse_lines (self, lines):
        for l in lines:
            self.parser.parse_line (l)

class Notation_item:
    """A single notation element (corresponds to a Grob in LilyPond)"""
    def __init__ (self):
        self.origin_tag = None
        self.bbox = None
        self.offset = (0,0)
        self.tag = None
        self.name = ''
        self.args = []
	self.canvas_item = None
	self.music_expression = None
        
class Notation:
    """A complete line/system/page of LilyPond output. Consists of a
    number of Notation_items"""
    
    def __init__ (self, controller):
        self.items = []
	self.notation_controller = controller
        self.touched = True
        self.cursor_touched = True
        
        toplevel = controller.document.music
        self.music_cursor = toplevel.find_first (lambda x: x.name()== "NoteEvent") 
        
    def get_document (self):
	return self.notation_controller.document
	
    def add_item (self, offset, cause, bbox, fields):
	    item = Notation_item()
 	    item.tag = fields[0]
	    item.args = map (eval, fields[1:])
	    item.offset = offset
	    item.origin_tag = cause
	    
	    if cause and cause >= 0:
		item.music_expression = self.get_document ().tag_dict[cause]
		
	    item.bbox = bbox
	    
	    self.items.append (item)
            return item
        
    def clear(self):
        self.items = [] 
            
    def paint_on_canvas (self,  canvas):
        for w in  canvas.root().item_list:
            if w.notation_item:
                w.destroy()
                
	for i in self.items:
	    c_item = canvas.create_canvas_item (i)

        canvas.set_cursor_to_music (self.music_cursor)
        self.touched = False
        
    def set_cursor (self, music_expr):
        self.music_cursor = music_expr
        self.cursor_touched = True
        self.ensure_cursor_visible ()
        
    def cursor_move (self, dir):
        mus = self.music_cursor
        if mus.parent.name() == 'EventChord':
            mus = mus.parent
        
        mus = mus.parent.get_neighbor (mus, dir)
        mus = mus.find_first (lambda x: x.name() in ('NoteEvent', 'RestEvent'))
        self.set_cursor (mus)

    def cursor_move_chord (self, dir):
        mus = self.music_cursor
        if mus.name ()=='NoteEvent':
            current_steps  = mus.pitch.steps()
            other_steps = [(note, note.pitch.steps()) for
                           note in mus.parent.elements if note.name()=='NoteEvent']

            def cmp(a,b):
                if a[1] > b[1]:
                    return 1
                if a[1] < b[1]:
                    return -1
                return 0

            bound_set = [(note, dir * step) for (note, step) in other_steps
                         if dir * (step - current_steps) > 0]
            bound_set.sort (cmp)
            if bound_set:
                self.set_cursor (bound_set[0][0])
              
    def insert_at_cursor (self, music, dir):
        mus = self.music_cursor
        if mus.parent.name() == 'EventChord':
            mus = mus.parent

     	mus.parent.insert_around (mus, music, dir)
        self.touch_document()

    def touch_document (self):
        self.get_document ().touched = True

    def check_update (self):
        if self.get_document().touched:
            self.notation_controller.update_notation ()
        
    def backspace (self):
        mus = self.music_cursor
        if (mus.parent.name() == 'EventChord'
            and len (mus.parent.elements) <= 1):
            
            mus = mus.parent

        neighbor = mus.parent.get_neighbor (mus, -1)
        mus.parent.delete_element (neighbor)
        self.touch_document ()
        
    def change_octave (self, dir):
        if self.music_cursor.name() == 'NoteEvent':
            p = self.music_cursor.pitch
            p.octave += dir 
        self.touch_document ()

    def set_step (self, step):
        self.ensure_note ()
        if self.music_cursor.name() == 'NoteEvent':

            # relative mode.
            p = self.music_cursor.pitch
            p1 = p.copy()
            p1.step = step
            p1.alteration = scale_alterations [step]
            
            orig_steps = p.steps ()
            new_steps = p1.steps ()
            diff = new_steps - orig_steps
            if diff >= 4:
                    p1.octave -= 1
            elif diff <= -4:
                    p1.octave += 1
                    
            self.music_cursor.pitch = p1
            self.touch_document ()

        else:
            print 'not a NoteEvent'

    def add_step (self, step):
        self.ensure_note ()
        if self.music_cursor.name() == 'NoteEvent':

            # relative mode.
            p = self.music_cursor.pitch
            p1 = p.copy()
            p1.step = step
            
            orig_steps = p.steps ()
            new_steps = p1.steps ()
            diff = new_steps - orig_steps
            if diff >= 4:
                    p1.octave -= 1
            elif diff <= -4:
                    p1.octave += 1

            new_ev = music.NoteEvent()
            new_ev.pitch = p1
            new_ev.duration = self.music_cursor.duration.copy()

            self.music_cursor.parent.insert_around (self.music_cursor,
                                                    new_ev, 1)
            self.music_cursor = new_ev
            self.touch_document ()
        else:
            print 'not a NoteEvent'

    def change_step (self, dstep):
        self.ensure_note ()
        if self.music_cursor.name() == 'NoteEvent':

            # relative mode.
            p = self.music_cursor.pitch
            p1 = p.copy()
            p1.step += dstep

            if p1.step > 6:
                p1.step -= 7
                p1.octave += 1
            elif p1.step < 0:
                p1.step += 7
                p1.octave -= 1

            p1.alteration = scale_alterations [p1.step]
                
            self.music_cursor.pitch = p1
            self.touch_document ()

        else:
            print 'not a NoteEvent'
            
    def change_duration_log (self, dir):
        if ( self.music_cursor.name() == 'NoteEvent'
             or self.music_cursor.name() == 'RestEvent'):

            dur = self.music_cursor.duration
            dl = dur.duration_log
            dl += dir
            if dl <= 6 and dl >= -2:
                dur.duration_log = dl
                
            self.touch_document ()


    def ensure_note (self):
        if self.music_cursor.name() == 'RestEvent':
            m = self.music_cursor
            note = music.NoteEvent()
            m.parent.insert_around (None, note, 1)
            m.parent.delete_element (m)
            self.music_cursor = note
            self.touch_document ()
            
    def ensure_rest (self):
        if self.music_cursor.name() == 'NoteEvent':
            m = self.music_cursor
            rest = music.RestEvent()
            m.parent.insert_around (None, rest, 1)
            m.parent.delete_element (m)
            self.music_cursor = rest  
            self.touch_document ()
            
    def change_dots (self):
        if self.music_cursor.name() == 'NoteEvent':
            p = self.music_cursor.duration
            if p.dots == 1:
                p.dots = 0
            elif p.dots == 0:
                p.dots = 1
            self.touch_document ()
            
    def ensure_cursor_visible(self):
        self.notation_controller.document.recompute()
        self.notation_controller.ensure_visible (self.music_cursor.start)

    def change_alteration (self, dir):
        if self.music_cursor.name() == 'NoteEvent':
            p = self.music_cursor.pitch

            new_alt = p.alteration + dir
            if abs (new_alt) <= 4: 
                p.alteration = new_alt
            self.touch_document ()

    def set_alteration (self, alter):
        if self.music_cursor.name() == 'NoteEvent':
            p = self.music_cursor.pitch
            p.alteration = alter
            self.touch_document ()

    def toggle_arpeggio (self):
        par = self.music_cursor.parent
        if par.name()== "EventChord":
            arps = [e for e in par.elements if e.name()=='ArpeggioEvent']
            if arps:
                par.delete_element (arps[0])
            else:
                arp = music.ArpeggioEvent()
                par.insert_around (self.music_cursor, arp, -1)
            self.touch_document()
    def print_score(self):
        doc = self.notation_controller.document
        ly = doc.music.ly_expression()
        render_score('score.ly', ly) 

    def add_note (self):
        if self.music_cursor.name () == 'NoteEvent':
            note = music.NoteEvent ()
            note.pitch = self.music_cursor.pitch.copy()
            note.duration = self.music_cursor.duration.copy()
            
            ch = music.EventChord ()
            ch.insert_around (None, note, 0)
            
            self.insert_at_cursor (ch, 1)
            self.cursor_move (1)
            self.touch_document ()

        elif self.music_cursor.name () == 'RestEvent':
            rest = music.RestEvent ()
            rest.duration = self.music_cursor.duration.copy()
            
            ch = music.EventChord ()
            ch.insert_around (None, rest, 0)
            
            self.insert_at_cursor (ch, 1)
            self.cursor_move (1)
            self.touch_document ()
            

