/*   
  break-align-engraver.cc --  implement Break_align_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "engraver.hh"
#include "protected-scm.hh"
#include "break-align-item.hh"
#include "axis-group-item.hh"

class Break_align_engraver : public Engraver
{
  Break_align_item *align_l_;
  Protected_scm column_alist_;
protected:
  virtual void acknowledge_element(Score_element_info i);
  virtual void do_pre_move_processing ();
  void add_column (SCM);
  
public:
  VIRTUAL_COPY_CONS(Translator);
  Break_align_engraver ();
};



ADD_THIS_TRANSLATOR(Break_align_engraver);

void
Break_align_engraver::add_column (SCM smob)
{
  Score_element * e = unsmob_element (smob);
  align_l_->add_element (e);
  typeset_element (e);
}

void
Break_align_engraver::do_pre_move_processing ()
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
      typeset_element (align_l_);
      align_l_ = 0;
    }
}


Break_align_engraver::Break_align_engraver ()
{
  column_alist_ = SCM_EOL;
  align_l_ =0;
}

void
Break_align_engraver::acknowledge_element (Score_element_info inf)
{
  if (Item * item_l = dynamic_cast <Item *> (inf.elem_l_))
    {
      if (item_l->empty_b (X_AXIS) || item_l->parent_l (X_AXIS))
	return;

      SCM bp=item_l->remove_elt_property ("breakable");
      bool breakable = (to_boolean (bp));
      if (!breakable)
	return ;

      SCM al = item_l->remove_elt_property ("break-aligned");
      if (!gh_boolean_p (al ) || !gh_scm2bool (al))
	return ;

      
      if (!align_l_)
	{
	  align_l_ = new Break_align_item;
	  align_l_->set_elt_property ("breakable", SCM_BOOL_T);
	  announce_element (Score_element_info (align_l_,0));
	}

      SCM name = ly_str02scm (inf.elem_l_->name());
      SCM s = scm_assoc (name, column_alist_);

      Axis_group_item * group = 0;
      if (s != SCM_BOOL_F)
	{
	  Score_element *e =  unsmob_element (gh_cdr(s));
	  group = dynamic_cast<Axis_group_item*> (e);
	}
      else
	{
	  group = new Axis_group_item;
	  group->set_axes (X_AXIS,X_AXIS);
	  group->set_elt_property ("origin", name);
	  group->set_parent (align_l_, Y_AXIS);
	  announce_element (Score_element_info (group, 0));
	  column_alist_ = scm_assoc_set_x (column_alist_, name, group->self_scm_);
	}
      group->add_element (item_l);
    }
}
