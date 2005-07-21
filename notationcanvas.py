import gtk
import gnomecanvas
import music
import math

class Notation_toolbar (gtk.VBox):
	def __init__ (self, notation, check_refresh_callback):
		gtk.VBox.__init__ (self)
		self.button_dict = {}
		self.key_dict = {}
		self.notation = notation
		self.rows = {}
		self.add_buttons ()
		self.check_refresh_callback = check_refresh_callback
		
	def get_row (self, row):
		if not self.rows.has_key (row):
			r = gtk.HBox()
			self.pack_start (r, expand = False)
			r.show ()
			self.rows[row] = r
			return r
		
		return self.rows[row]
		
	def click_callback (self, widget):
		if not self.button_dict.has_key (widget):
			print 'no such widget?'
			return False

		cb = self.button_dict[widget]
		cb()
		self.check_refresh_callback()
		return True

	def keypress_callback (self, widget, event):
		key =  event.keyval
		name = gtk.gdk.keyval_name (key)

		if event.get_state () & gtk.gdk.SHIFT_MASK:
			name = 'Shift+' + name 
		if event.get_state () & gtk.gdk.CONTROL_MASK:
			name = 'Ctrl+' + name
		if not self.key_dict.has_key (name):
			print 'no such key?', name
			return False

		button = self.key_dict[name]
		button.do_activate (button)
		return True
	
	def add_button (self, text, key, callback, row_id):
		b = gtk.Button (text)
		row = self.get_row (row_id)
		row.pack_start (b, expand=True)
		
		b.connect ('clicked', self.click_callback)
		b.set_focus_on_click (False)
		self.key_dict[key] = b
		self.button_dict[b] = callback
		b.show ()

	def add_buttons (self):
		for (key_name, text, func, row) in \
			[('p', 'LilyPond',
			  lambda: self.notation.print_score(), 0),
			 ('q', 'quit',
			  lambda: gtk.main_quit(), 0),
			 ('Left', '<-',
			  lambda: self.notation.cursor_move (-1), 0),
			 ('Right', '->',
			  lambda: self.notation.cursor_move (1), 0),
			 ('space', 'new',
			  lambda: self.notation.add_note (), 0),
			 ('BackSpace', 'backspace',
			  lambda: self.notation.backspace (), 0),
			 ('Shift+Up', '#',
			  lambda: self.notation.change_alteration (2), 1),
			 ('Shift+Down', 'b',
			  lambda: self.notation.change_alteration (-2), 1),
			 ('Up', 'up',
			  lambda: self.notation.change_step (1), 1),
			 ('Down', 'down',
			  lambda: self.notation.change_step (-1), 1),
			 ('apostrophe', 'oct up',
			  lambda: self.notation.change_octave (1), 1),
			 ('comma', 'oct down',
			  lambda: self.notation.change_octave (-1), 1),
			 ('period', '.',
			  lambda: self.notation.change_dots (), 1),
			 ('slash', 'shorter',
			  lambda: self.notation.change_duration_log (1), 1),
			 ('Shift+asterisk', 'longer',
			  lambda: self.notation.change_duration_log (-1), 1),
			 ('r', 'rest',
			  lambda: self.notation.ensure_rest (), 1),
			 ('Shift+C', '+C',
			  lambda: self.notation.add_step (0), 2),
			 ('Shift+D', '+D',
			  lambda: self.notation.add_step (1), 2),
			 ('Shift+E', '+E',
			  lambda: self.notation.add_step (2), 2),
			 ('Shift+F', '+F',
			  lambda: self.notation.add_step (3), 2),
			 ('Shift+G', '+G',
			  lambda: self.notation.add_step (4), 2),
			 ('Shift+A', '+A',
			  lambda: self.notation.add_step (5), 2),
			 ('Shift+B', '+B',
			  lambda: self.notation.add_step (6), 2),
			 ('c', 'C',
			  lambda: self.notation.set_step (0), 3),
			 ('d', 'D',
			  lambda: self.notation.set_step (1), 3),
			 ('e', 'E',
			  lambda: self.notation.set_step (2), 3),
			 ('f', 'F',
			  lambda: self.notation.set_step (3), 3),
			 ('g', 'G',
			  lambda: self.notation.set_step (4), 3),
			 ('a', 'A',
			  lambda: self.notation.set_step (5), 3),
			 ('b', 'B',
			  lambda: self.notation.set_step (6), 3)]:
			
			self.add_button (text, key_name, func, row)


class Notation_canvas (gnomecanvas.Canvas):
	"""The canvas for drawing notation symbols."""
	
	def __init__ (self, canvas_controller):
		gnomecanvas.Canvas.__init__ (self,
					     #aa=True
					     )
		(w,h) = (400,200)
		self.set_size_request (w, h) 
		self.set_scroll_region (0, 0, w, h)
		root = self.root ()
		root.affine_relative ((1,0,0,-1,0, 5))
		self.pixel_scale = 10
		self.set_pixels_per_unit (self.pixel_scale)
		i = root.add (gnomecanvas.CanvasRect,
			      y1 = 5,
			      x2 = w, y2 = -h + 5,
			      fill_color = 'white', outline_color = 'white')
		i.notation_item = None
		self.notation_canvas_controller = canvas_controller
		self.create_cursor ()
		
	def create_cursor (self):
		type = gnomecanvas.CanvasRect
		w = self.root ().add (type,
				      fill_color = 'lightblue',
				      outline_color = 'lightblue')
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
		
	def click_event (self, widget, event = None):
		if event <> None and event.type == gtk.gdk.BUTTON_PRESS:
			if event.button == 1 and widget.notation_item.name in ('Rest', 'NoteHead'):
				
				notat = self.notation_canvas_controller.notation
				notat.set_cursor (widget.notation_item.music_expression)
				self.notation_canvas_controller.check_update()
				
				return True
		return False
	
	def register_notation_canvas_item (self, citem):
		if citem.notation_item and citem.notation_item.music_expression:
			citem.connect ("event", self.click_event)

	def set_cursor_to_music (self, music_expression):
		c_items = [it for it in self.root().item_list if
			   (it.notation_item
			    and it.notation_item.music_expression
			        == music_expression)]

		c_items = filter (lambda it: it.notation_item.name in ('NoteHead', 'Rest'),
				  c_items) 
		if c_items:
			self.set_cursor (c_items[0].notation_item)

	def zoom (self, delta):
		fact = pow (1.25, delta)
		self.pixel_scale *= fact
		self.set_pixels_per_unit (self.pixel_scale)
		
class Notation_canvas_controller:
	"""The connection between canvas and the abstract notation graphics."""

	def __init__ (self, notation):
		self.canvas = Notation_canvas (self)
		self.notation = notation
		
	def update_cursor (self):
		self.canvas.set_cursor_to_music (self.notation.music_cursor)

	def update_canvas (self):
		self.notation.paint_on_canvas (self.canvas)

	def check_update (self):
		self.notation.check_update ()
		if self.notation.touched:
			self.update_canvas ()
		elif self.notation.cursor_touched:
			self.update_cursor ()

	
