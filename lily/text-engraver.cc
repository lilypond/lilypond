/*   
  text-engraver.cc --  implement Text_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "dimension-cache.hh"
#include "engraver.hh"
#include "side-position-interface.hh"
#include "item.hh"
#include "musical-request.hh"

#include "stem.hh"
#include "staff-symbol.hh"

/**
   typeset directions that are  plain text.
 */
class Text_engraver : public Engraver
{
  Link_array<Text_script_req> reqs_;
  Link_array<Item> texts_;
public:
  VIRTUAL_COPY_CONS(Translator);
protected:
  virtual bool do_try_music (Music* m);
  virtual void do_pre_move_processing ();
  virtual void do_post_move_processing ();
  virtual void do_process_music ();
  virtual void acknowledge_element (Score_element_info);
};


bool
Text_engraver::do_try_music (Music *m)
{
  if (Text_script_req *r = dynamic_cast<Text_script_req*> (m))
    {
      reqs_.push (r);
      return true;
    }
  return false;
}


void
Text_engraver::acknowledge_element (Score_element_info inf)
{
  if (to_boolean (inf.elem_l_->get_elt_property ("note-head-interface")))
    {
      for (int i=0; i < texts_.size (); i++)
	{
	  Side_position_interface st (texts_[i]);
	  st.add_support (inf.elem_l_);
	  if (st.get_axis( ) == X_AXIS
	      && !texts_[i]->parent_l (Y_AXIS))
	    texts_[i]->set_parent (inf.elem_l_, Y_AXIS);
	}
    }
  if (Stem *n = dynamic_cast<Stem*> (inf.elem_l_))
    {
      for (int i=0; i < texts_.size (); i++)
	{
	  Side_position_interface st(texts_[i]);
	  st.add_support (n);
	}
    }
}

void
Text_engraver::do_process_music ()
{
  for (int i=0; i < reqs_.size (); i++)
    {
      Text_script_req * r = reqs_[i];

      Item *text = new Item (get_property ("basicTextScriptProperties"));
      Side_position_interface stafy (text);

      SCM axisprop = get_property ("scriptHorizontal");
      if (to_boolean (axisprop))
	{
	  stafy.set_axis (X_AXIS);
	  //	  text->set_parent (ss, Y_AXIS);
	}
      else
	stafy.set_axis (Y_AXIS);

      /*
	make sure they're in order by adding i to the priority field.
	*/
      text->set_elt_property ("script-priority",
			    gh_int2scm (200 + i));

      if (r->get_direction ())
	stafy.set_direction (r->get_direction ());
      
      text->set_elt_property ("text",
			      ly_str02scm ( r->text_str_.ch_C ()));
      
      if (r->style_str_.length_i ())
	text->set_elt_property ("style", ly_str02scm (r->style_str_.ch_C()));
      
      SCM empty = get_property ("textNonEmpty");
      if (to_boolean (empty))
	{
	  text->set_elt_property ("no-spacing-rods" , SCM_BOOL_F);
	  text->set_extent_callback (0, X_AXIS);
	}
      announce_element (Score_element_info (text, r));
      texts_.push (text);
    }
}

void
Text_engraver::do_pre_move_processing ()
{
  for (int i=0; i < texts_.size (); i++)
    {
      Item *ti = texts_[i];
      Side_position_interface (ti).add_staff_support ();
      typeset_element (ti);
    }
  texts_.clear ();
}

void
Text_engraver::do_post_move_processing ()
{
  reqs_.clear ();
}

ADD_THIS_TRANSLATOR(Text_engraver);

