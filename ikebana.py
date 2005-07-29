#!/usr/bin/env python

import musictree
import os
import gtk
import gnomecanvas
import random
import string
import notation
import notationcanvas
import music

def mainquit (*args):
    gtk.main_quit ()

class NotationApplication:
    def __init__ (self):
        self.document = music.Music_document ()
        
        nc = notation.Notation_controller (self.document)
        self.notation_controller = nc

        ncc = notationcanvas.Notation_canvas_controller (nc.notation)
        self.notation_canvas_controller = ncc
        
        self.window = self.create_window ()
        self.tree_window =self.create_tree_window ()

    def tree_selection_changed (self, music_obj):
        nc = self.notation_controller
        nc.notation.set_cursor (music_obj)
        self.notation_canvas_controller.check_update()
        
    def create_tree_window (self):
        win = gtk.Window ()
        (w,h) = (500,300)
        win.set_size_request (w, h) 
        
        win.connect ('destroy', mainquit)
        win.set_title ('Ikebana - music representation')
        
        treeview = musictree.MusicTreeView (self.document.music)
        win.add(treeview)
        win.show()
        treeview.selection_change_callback = self.tree_selection_changed

        notation = self.notation_canvas_controller.notation
        notation.set_cursor_callback = treeview.cursor_changed
        
        return win

    def create_window (self):
        win = gtk.Window ()
        win.connect ('destroy', mainquit)
        win.set_title ('Ikebana - visual music notation')

        canvas = self.notation_canvas_controller.canvas
        canvas.show ()

        tb_type = notationcanvas.Notation_toolbar 
        toolbar = tb_type (self.notation_canvas_controller.notation,
                           self.notation_canvas_controller.check_update
                           )

        canvas.connect ("key-press-event", toolbar.keypress_callback)

        vbox = gtk.VBox ()
        vbox.pack_start (canvas, expand=True)
        vbox.pack_start (toolbar, expand=False)
        vbox.show ()
        
        win.add (vbox)
        toolbar.show ()

        win.show()
        
        return win
        
    def main (self):
        self.notation_controller.update_notation ()
        self.notation_canvas_controller.update_canvas ()

if __name__ == '__main__':
    c = NotationApplication ()
    c.main ()
    gtk.main ()
