/*   
  text-engraver.cc --  implement Text_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "dimension-cache.hh"

#include "engraver.hh"
#include "side-position-interface.hh"
#include "text-item.hh"
#include "musical-request.hh"
#include "note-head.hh"
#include "stem.hh"
#include "staff-symbol.hh"

/**
   typeset directions that are  plain text.
 */
class Text_engraver : public Engraver
{
  Link_array<Text_script_req> reqs_;
  Link_array<Text_item> texts_;
public:

  VIRTUAL_COPY_CONS(Translator);
protected:
  virtual bool do_try_music (Music* m);
  virtual void do_pre_move_processing ();
  virtual void do_post_move_processing ();
  virtual void do_process_requests ();
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
Text_engraver::acknowledge_element (Score_element_info i)
{
  if (Note_head *n = dynamic_cast<Note_head*> (i.elem_l_))
    {
      for (int i=0; i < texts_.size (); i++)
	{
	  Side_position_interface st (texts_[i]);
	  st.add_support (n);
	  if (st.get_axis( ) == X_AXIS
	      && !texts_[i]->parent_l (Y_AXIS))
	    texts_[i]->set_parent (n, Y_AXIS);
	}
    }
  if (Stem *n = dynamic_cast<Stem*> (i.elem_l_))
    {
      for (int i=0; i < texts_.size (); i++)
	{
	  Side_position_interface st(texts_[i]);
	  st.add_support (n);
	}
    }
}

void
Text_engraver::do_process_requests ()
{
  for (int i=0; i < reqs_.size (); i++)
    {
      Text_script_req * r = reqs_[i];

      Text_item *text = new Text_item;
      Side_position_interface stafy (text);

      SCM axisprop = get_property ("scriptHorizontal",0);
      if (to_boolean (axisprop))
	{
	  stafy.set_axis (X_AXIS);
	  //	  text->set_parent (ss, Y_AXIS);
	}
      else
	stafy.set_axis (Y_AXIS);
      
      text->set_elt_property ("script-priority",
			    gh_int2scm (200));

      if (r->get_direction ())
	stafy.set_direction (r->get_direction ());
      
      text->set_elt_property ("text",
			      ly_str02scm ( r->text_str_.ch_C ()));
      
      if (r->style_str_.length_i ())
	text->set_elt_property ("style", ly_str02scm (r->style_str_.ch_C()));
      
      SCM empty = get_property ("textEmptyDimension", 0);
      if (to_boolean (empty))
	{
	  text->set_empty (X_AXIS);
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
      typeset_element (texts_[i]);
    }
  texts_.clear ();
}

void
Text_engraver::do_post_move_processing ()
{
  reqs_.clear ();
}

ADD_THIS_TRANSLATOR(Text_engraver);

