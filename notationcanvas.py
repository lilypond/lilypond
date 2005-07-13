import gtk
import gnomecanvas
import music

class Notation_canvas (gnomecanvas.Canvas):
	"""The canvas for drawing notation symbols."""
	
	def __init__ (self):
		gnomecanvas.Canvas.__init__ (self,
					     #aa=True
					     )
		w,h = 800,400
		self.set_size_request (w, h) 
		self.set_scroll_region (0, 0, w, h)
		root = self.root ()
		root.affine_relative ((1,0,0,-1,0,0))
		self.pixel_scale = 10
		self.set_pixels_per_unit (self.pixel_scale)
		i = root.add (gnomecanvas.CanvasRect, x2 = w, y2 = -h,
			      fill_color = 'white', outline_color = 'white')
		i.notation_item = None
		self.create_cursor ()
		
	def create_cursor (self):
		type = gnomecanvas.CanvasRect
		w = self.root ().add (type,
				      fill_color = 'None',
				      outline_color = 'blue')
		w.notation_item = None
		
		self.cursor_widget = w
		
	def set_cursor (self, notation_item):
		if not notation_item.bbox:
			print 'no bbox'
			return

		(x1, y1, x2, y2) = notation_item.bbox
		self.cursor_widget.set (x1 = x1,
					x2 = x2,
					y1 = y1,
					y2 = y2)
		
		
	def item_set_active_state (self, item, active):
		color = 'black'
		if active:
			color = 'red'
			
		item.set (fill_color = color)
		
	def item_event (self, widget, event=None):
		if 0:
			pass
		elif event.type == gtk.gdk.ENTER_NOTIFY:
			self.item_set_active_state(widget, True)
			return True
		elif event.type == gtk.gdk.LEAVE_NOTIFY:
			self.item_set_active_state(widget, False)
			return True
		return False
	
	def register_notation_canvas_item (self, citem):
		if citem.notation_item and citem.notation_item.music_expression:
			citem.connect ("event", self.item_event)

	def set_cursor_to_music (self, music_expression):
		c_items = [it for it in self.root().item_list if
			   (it.notation_item
			    and it.notation_item.music_expression
			        == music_expression)]

		if c_items:
			self.set_cursor (c_items[0].notation_item)

class Notation_canvas_controller:
	"""The connection between canvas and the abstract notation graphics."""

	def __init__ (self, notation):
		self.canvas = Notation_canvas ()
		self.notation = notation
		self.canvas.connect ("key-press-event", self.keypress)

	def update_cursor (self):
		self.canvas.set_cursor_to_music (self.notation.music_cursor)

	def update_canvas (self):
		self.notation.paint_on_canvas (self.canvas)
	
	def add_note (self):
		note = music.NoteEvent ()
		if self.notation.music_cursor.name () == 'NoteEvent':
			note.pitch = self.notation.music_cursor.pitch.copy()
			note.pitch.alteration = 0
			note.duration = self.notation.music_cursor.duration.copy()
		
		ch = music.EventChord ()
		ch.insert_around (None, note, 0)
		
		self.notation.insert_at_cursor (ch, 1)
		self.notation.cursor_move (1)
		
	def keypress (self, widget, event):
		key =  event.keyval
		name = gtk.gdk.keyval_name (key)

		if 0:
			pass
		elif name == 'Left':
			self.notation.cursor_move (-1)
			self.update_cursor ()
			
		elif name == 'Right':
			self.notation.cursor_move (1)
			self.update_cursor ()
		elif name == 'space':
			self.add_note ()
		elif name == 'apostrophe':
			self.notation.change_octave (1)
		elif name == 'comma':
			self.notation.change_octave (-1)
		elif name == 'BackSpace':
			self.notation.backspace ()
		elif name == 'plus':
			self.notation.change_alteration (2)
		elif name == 'minus':
			self.notation.change_alteration (-2)
		elif name == 'period':
			self.notation.change_dots ()
		elif name == 'slash':
			self.notation.change_duration_log (1)
		elif name == 'asterisk':
			self.notation.change_duration_log (-1)
		
		elif name == 'r':
			self.notation.ensure_rest ()
		elif len (name) == 1 and name in 'abcdefg':
			step = (ord (name) - ord ('a') + 5) % 7
			self.notation.ensure_note ()
			self.notation.change_step (step)
		else:
			print 'Unknown key %s' % name
			return False
		
		self.notation.notation_controller.update_notation ()
		self.notation.paint_on_canvas (self.canvas)
		return True

