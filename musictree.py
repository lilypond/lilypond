#!/usr/bin/env python
'''Tree View/Generic Tree Model

This test is designed to demonstrate creating a new type of tree model
in python for use with the new tree widget in gtk 2.0.'''

import gtk
import gobject

# to create a new GtkTreeModel from python, you must derive from
# TreeModel.
class MusicTreeModel(gtk.GenericTreeModel):
    '''This class represents the model of a tree.  The iterators used
    to represent positions are converted to python objects when passed
    to the on_* methods.  This means you can use any python object to
    represent a node in the tree.  The None object represents a NULL
    iterator.

    In this tree, we use simple tuples to represent nodes, which also
    happen to be the tree paths for those nodes.  This model is a tree
    of depth 3 with 5 nodes at each level of the tree.  The values in
    the tree are just the string representations of the nodes.'''

    def __init__(self, music):
        '''constructor for the model.  Make sure you call
        PyTreeModel.__init__'''
        gtk.GenericTreeModel.__init__(self)
	self.music = music
	
    # the implementations for TreeModel methods are prefixed with on_
    def on_get_flags(self):
        '''returns the GtkTreeModelFlags for this particular type of model'''
        return 0

    def on_get_n_columns(self):
        '''returns the number of columns in the model'''
        return 2

    def on_get_column_type(self, index):
        '''returns the type of a column in the model'''
        return gobject.TYPE_STRING

    def on_get_path(self, node):
        '''returns the tree path(a tuple of indices at the various
        levels) for a particular node.'''

#	print 'on_get_path'  
	if node.parent:
		return tuple (list (self.on_get_path (node.parent))
			      +
			      [node.get_index ()])
	else:
		return (0,)
		
    def on_get_iter(self, path):
        '''returns the node corresponding to the given path.  In our
        case, the node is the path'''

#	print 'on_get_iter'  , path

	ptr = self.music
	for i in path[1:]:
		ptr = ptr.elements[i]
		
        return ptr

    def on_get_value(self, node, column):
        '''returns the value stored in a particular column for the node'''

	str = ''
	if column == 0:
		str = node.name()
	elif column==1:
		str = node.ly_expression()
		
        return str

    def on_iter_next(self, node):
        '''returns the next node at this level of the tree'''
#	print 'on_iter_next', node
	if not node.parent:
		return None 

	neigh = node.parent.get_neighbor (node, 1)
	if neigh <> node:
		return neigh
	else:
		return None
	
    def on_iter_children(self, node):
        '''returns the first child of this node'''
#	print 'on_iter_children', node
	if node.has_children ():
		return node.elements[0]
	else:
		return None

    def on_iter_has_child(self, node):
        '''returns true if this node has children'''
	hc = node.has_children ()
	
#	print node, 'has_child', hc

	return hc

    def on_iter_n_children(self, node):
        '''returns the number of children of this node'''
	assert  node <> None
		
#	print 'on_iter_n_children', node 

	return len (node.elements)
	    
    def on_iter_nth_child(self, node, n):
        '''returns the nth child of this node'''
        if node == None:
            return self.music
    
#	print 'on_iter_nth_child', node , n

        return node.elements[n]

    def on_iter_parent(self, node):
        '''returns the parent of this node'''
	
#	print 'on_iter_parent', node 

	return node.parent

class MusicTreeView(gtk.ScrolledWindow):
    def __init__(self, music):
        gtk.ScrolledWindow.__init__(self)
        self.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)

        model = MusicTreeModel (music)
	self.model = model
        tree_view = gtk.TreeView(model)
        cell = gtk.CellRendererText()
	
        # the text in the column comes from column 0
        column = gtk.TreeViewColumn("Type", cell, text=0)
        tree_view.append_column(column)
        column = gtk.TreeViewColumn("Contents", cell, text=1)
        tree_view.append_column(column)
	self.tree_selection = tree_view.get_selection()

        self.add(tree_view)
        self.show_all()

	self.tree_selection.connect ('changed', self.selection_changed)
	self.selection_change_callback = None
	
    def selection_changed (self, a):
	    if self.selection_change_callback:
		    (model, iter) = a.get_selected()
		    path = model.get_path (iter)
		    obj = model.on_get_iter (path)
		    if obj.name() in ('RestEvent', 'NoteEvent'):
			    self.selection_change_callback(obj)
	    
    def select_music (self, music):
	    path = self.model.on_get_path (music)
	    self.tree_selection.select_path (path)
	    
    def cursor_changed (self, notation):
	    self.select_music (notation.music_cursor)
	    
def main():
    GenericTreeModelDemo()
    gtk.main()

if __name__ == '__main__':
    main()

