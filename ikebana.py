#!/usr/bin/env python

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
        self.music = music.Music_document ()
        
        nc = notation.Notation_controller (self.music)
        self.notation_controller = nc

        ncc = notationcanvas.Notation_canvas_controller (nc.notation)
        self.notation_canvas_controller = ncc
        
        self.window = self.create_window ()
        
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
