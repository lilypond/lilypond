/*   
  break-align-engraver.cc --  implement Break_align_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "engraver.hh"
#include "protected-scm.hh"
#include "break-align-item.hh"
#include "item.hh"
#include "align-interface.hh"
#include "axis-group-interface.hh"


class Break_align_engraver : public Engraver
{
  Item *align_l_;
  Protected_scm column_alist_;
protected:
  virtual void finalize ();
  virtual void acknowledge_grob(Grob_info i);
  virtual void stop_translation_timestep ();
  void add_column (SCM);
  
public:
  VIRTUAL_COPY_CONS(Translator);
  Break_align_engraver ();
};



ADD_THIS_TRANSLATOR(Break_align_engraver);

void
Break_align_engraver::add_column (SCM smob)
{
  Grob * e = unsmob_grob (smob);
  Break_align_interface::add_element (align_l_,e);
  typeset_grob (e);
}

void
Break_align_engraver::finalize ()
{
  column_alist_ = SCM_EOL;
}

void
Break_align_engraver::stop_translation_timestep ()
{
  SCM order = get_property ("breakAlignOrder");
  for (; gh_pair_p (order); order = gh_cdr (order))
    {
      SCM p = scm_assoc ( gh_car (order), column_alist_);
      if (gh_pair_p (p))
	{
	  add_column (gh_cdr (p));
	  column_alist_ = scm_assoc_remove_x (column_alist_, gh_car (order));
	}
    }

  for (SCM p = column_alist_; gh_pair_p (p); p = gh_cdr (p))
    {
      SCM pair = gh_car (p);
      add_column (gh_cdr (pair));
    }

  
  column_alist_ = SCM_EOL;

  if (align_l_)
    {
      typeset_grob (align_l_);
      align_l_ = 0;
    }
}


Break_align_engraver::Break_align_engraver ()
{
  column_alist_ = SCM_EOL;
  align_l_ =0;
}

void
Break_align_engraver::acknowledge_grob (Grob_info inf)
{
  if (Item * item_l = dynamic_cast <Item *> (inf.elem_l_))
    {
      if (item_l->empty_b (X_AXIS) || item_l->parent_l (X_AXIS))
	return;

      SCM bp=item_l->get_grob_property ("breakable");
      bool breakable = (to_boolean (bp));
      if (!breakable)
	return ;

      SCM align_name = item_l->get_grob_property ("break-align-symbol");
      if (!gh_symbol_p (align_name))
	return ;

      if (!align_l_)
	{
	  align_l_ = new Item (get_property ("BreakAlignment"));
	  Break_align_interface::set_interface (align_l_);
	  announce_grob (align_l_,0);

	  SCM edge_sym = ly_symbol2scm ("Left_edge_item");
	  Item * edge = new Item (get_property ("LeftEdge"));

	  /*
	    We must have left-edge in the middle.  Instrument-names
	    are left to left-edge, so they don't enter the staff.
	  */
	  align_l_->set_grob_property ("self-alignment-X", edge->self_scm ());
	  

	  /*
	    If the element is empty, it will be ignored in the break
	    alignment stuff.

	    TODO: switch off ignoring empty stuff?
	  */
	  edge->set_extent_callback (Grob::point_dimension_callback_proc, X_AXIS);
	  
	  align_l_->set_grob_property ("self-alignment-X", edge->self_scm ());

	  announce_grob (edge, 0);
	  column_alist_ = scm_assoc_set_x (column_alist_, edge_sym, edge->self_scm ());
	}

      SCM s = scm_assoc (align_name, column_alist_);

      Item * group = 0;

      if (s != SCM_BOOL_F)
	{
	  Grob *e =  unsmob_grob (gh_cdr(s));
	  group = dynamic_cast<Item*> (e);
	}
      else
	{
	  group = new Item (get_property ("BreakAlignGroup"));

	  Axis_group_interface::set_interface (group);
	  Axis_group_interface::set_axes (group, X_AXIS,X_AXIS);

	  group->set_grob_property ("break-align-symbol", align_name);
	  group->set_parent (align_l_, Y_AXIS);
	  announce_grob (group, 0);
	  column_alist_ = scm_assoc_set_x (column_alist_, align_name, group->self_scm ());

	}
      Axis_group_interface::add_element (group, item_l);
    }
}
