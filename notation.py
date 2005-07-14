import string
import re
import gnomecanvas
import gtk
import os
import socket
import music
import pango
import math

def talk_to_lilypond (expression_str):
    """Send a LISP expression to LilyPond, wait for return value."""
    sock = socket.socket (socket.AF_INET)
    address = ("localhost", 2904)
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
       (make-property-set 'currentBarNumber %d) 'Score)
    %s))""" % (num,str)

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
            self.name = fields[2]
	    self.bbox = tuple (map (string.atof, fields[3:]))
            return

        return self.interpret_socket_line (offset, self.cause_tag,
					   self.bbox, fields)

class Notation_controller:
    """Couple Notation and the music model. Stub for now. """
    def __init__ (self, music_document):
	self.document = music_document
        
        self.notation = Notation (self)
        self.parser = Lilypond_socket_parser (self.interpret_line)
        self.start_moment = 0.0
        self.stop_moment = 3.0

    def interpret_line (self, offset, cause, bbox, fields):
	notation_item = self.notation.add_item (offset, cause, bbox, fields)
    
    def update_notation(self):
	doc = self.document
	doc.recompute()
	
	expr = doc.music
        print 'subexp', self.start_moment, self.stop_moment

       	def sub(x):
		ok = (x.start >= self.start_moment and
                      x.start +x.length() <= self.stop_moment)
		return ok

        
	str = expr.lisp_sub_expression (sub)
        str = set_measure_number (str, int (self.start_moment) + 1)
        print str
	str = talk_to_lilypond (str)
        self.parse_socket_file (str)

    def ensure_visible (self, when):
        print when
        self.start_moment = max (math.floor (when - 1.0), 0.0)
        self.stop_moment = self.start_moment + 3.0
       
    def parse_socket_file (self, str):
        self.notation.clear ()
        lines = string.split (str, '\n')
        self.parse_lines (lines)
        
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
        self.args = []
	self.canvas_item = None
	self.music_expression = None
	
    def create_round_box_canvas_item (self, canvas):
	root = canvas.root()
	type = gnomecanvas.CanvasRect
	(b, w, d, h, blot) = tuple (self.args) 
	w = root.add (type,
		      fill_color = 'black',
		      x1 = - b,
		      y1 = - d,
		      x2 = w,
		      y2 = h)
	
	return w
	
    def create_line_canvas_item (self, canvas):
        type = gnomecanvas.CanvasLine
	(thick, x1, y1, x2, y2) = tuple (self.args)
        w = canvas.root().add (type,
			       fill_color = 'black',
			       width_units = thick,
			       points = [x1, y1, x2, y2]
			       )
        return w
 
    def create_glyph_item (self, canvas):
        type = gnomecanvas.CanvasText 
	(index, font_name, magnification, name) = tuple (self.args)
        (family, style) = string.split (font_name, '-')
        
        w = canvas.root().add (type,
			       fill_color = 'black',
			       family = family,
			       family_set = True,
			       anchor = gtk.ANCHOR_WEST,
			       y_offset = 0.15,
			       
			       size_points = canvas.pixel_scale * 0.75 * magnification,  
			       x = 0, y = 0,
			       text = unichr (index))
	return w


    def create_polygon_item (self, canvas):
        type = gnomecanvas.CanvasPolygon
        
        (blot, fill) = tuple (self.args[:2])
        coords = self.args[2:]
        w = canvas.root ().add (type,
                                fill_color = 'black',
                                width_units = blot,
                                points = coords)

        return w

    def create_text_item (self, canvas):
        type = gnomecanvas.CanvasText
        (descr, str) = tuple (self.args)

        magnification = 0.5

#ugh: how to get pango_descr_from_string() in pygtk?

	(fam,rest) = tuple (string.split (descr, ','))
        size = string.atof (rest)
        w = canvas.root().add (type,
                               fill_color = 'black',
                               family_set = True,
                               family = fam,
                               anchor = gtk.ANCHOR_WEST,
                               y_offset = 0.15,
                               size_points = size  * canvas.pixel_scale * 0.75 * magnification,
                               text = str)
        return w
        
    def create_canvas_item (self, canvas):
	dispatch_table = {'draw_round_box' : Notation_item.create_round_box_canvas_item,
			  'drawline': Notation_item.create_line_canvas_item,
			  'glyphshow': Notation_item.create_glyph_item,
                          'polygon': Notation_item.create_polygon_item,
                          'utf-8' : Notation_item.create_text_item,
			  }

	citem = None
	try:
	    method = dispatch_table[self.tag]
	    citem = method (self, canvas)
	    citem.move (*self.offset)
	    citem.notation_item = self

	    canvas.register_notation_canvas_item (citem)
	except KeyError:
	    print 'no such key', self.tag
	    
	return citem

	    
class Notation:

    """A complete line/system/page of LilyPond output. Consists of a
    number of Notation_items"""
    
    def __init__ (self, controller):
        self.items = []
	self.notation_controller = controller

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

    
    def clear(self):
        self.items = [] 
	
    def paint_on_canvas (self,  canvas):
        for w in  canvas.root().item_list:
            if w.notation_item:
                w.destroy()
                
	for i in self.items:
	    c_item = i.create_canvas_item (canvas)

        canvas.set_cursor_to_music (self.music_cursor)

    def cursor_move (self, dir):
        mus = self.music_cursor
        if mus.parent.name() == 'EventChord':
            mus = mus.parent
        
        mus = mus.parent.get_neighbor (mus, dir)
        mus = mus.find_first (lambda x: x.name() in ('NoteEvent', 'RestEvent'))
        self.music_cursor = mus
        
    def insert_at_cursor (self, music, dir):
        mus = self.music_cursor
        if mus.parent.name() == 'EventChord':
            mus = mus.parent

     	mus.parent.insert_around (mus, music, dir)

    def backspace (self):
        mus = self.music_cursor
        if mus.parent.name() == 'EventChord':
            mus = mus.parent

        neighbor = mus.parent.get_neighbor (mus, -1)
        mus.parent.delete_element (neighbor)

    def change_octave (self, dir):
        if self.music_cursor.name() == 'NoteEvent':
            p = self.music_cursor.pitch
            p.octave += dir 

    def change_step (self, step):
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
                    
            self.music_cursor.pitch = p1

        else:
            print 'not a NoteEvent'
            
    def change_duration_log (self, dir):
        if ( self.music_cursor.name() == 'NoteEvent'
             or self.music_cursor.name() == 'RestEvent'):

            dur = self.music_cursor.duration
            dl = dur.duration_log
            dl += dir
            if dl <= 6 and dl >= -3:
                dur.duration_log = dl
            

    def ensure_note (self):
        if self.music_cursor.name() == 'RestEvent':
            m = self.music_cursor
            note = music.NoteEvent()
            m.parent.insert_around (None, note, 1)
            m.parent.delete_element (m)
            self.music_cursor = note
            
    def ensure_rest (self):
        if self.music_cursor.name() == 'NoteEvent':
            m = self.music_cursor
            rest = music.RestEvent()
            m.parent.insert_around (None, rest, 1)
            m.parent.delete_element (m)
            self.music_cursor = rest  
            
    def change_dots (self):
        if self.music_cursor.name() == 'NoteEvent':
            p = self.music_cursor.duration
            if p.dots == 1:
                p.dots = 0
            elif p.dots == 0:
                p.dots = 1
            
    def ensure_cursor_visible(self):
        self.notation_controller.document.recompute()
        self.notation_controller.ensure_visible (self.music_cursor.start)

    def change_alteration (self, dir):
        if self.music_cursor.name() == 'NoteEvent':
            p = self.music_cursor.pitch

            new_alt = p.alteration + dir
            if abs (new_alt) <= 4: 
                p.alteration = new_alt

    def print_score(self):
        doc = self.notation_controller.document
        ly = doc.music.ly_expression()
        print ly
