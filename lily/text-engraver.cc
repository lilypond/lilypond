/*   
  text-engraver.cc --  implement Text_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "dimension-cache.hh"

#include "engraver.hh"
#include "staff-side.hh"
#include "text-item.hh"
#include "musical-request.hh"
#include "note-head.hh"
#include "stem.hh"
#include "staff-symbol.hh"

class Text_engraver : public Engraver
{
  Link_array<Text_script_req> reqs_;
  Link_array<Staff_side_item> positionings_;
  Link_array<Text_item> texts_;
public:
  Text_engraver();
  VIRTUAL_COPY_CONS(Translator);
protected:
  virtual bool do_try_music (Music* m);
  virtual void do_pre_move_processing ();
  virtual void do_post_move_processing ();
  virtual void do_process_requests ();
  virtual void acknowledge_element (Score_element_info);
};

Text_engraver::Text_engraver ()
{
  
}

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
      for (int i=0; i < positionings_.size (); i++)
	{
	  positionings_[i]->add_support (n);
	  if (positionings_[i]->axis_ == X_AXIS
	      && !positionings_[i]->parent_l (Y_AXIS))
	    positionings_[i]->set_parent (n, Y_AXIS);
	}
    }
  if (Stem *n = dynamic_cast<Stem*> (i.elem_l_))
    {
      for (int i=0; i < positionings_.size (); i++)
	{
	  positionings_[i]->add_support (n);
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
      Staff_side_item *ss = new Staff_side_item;



      SCM axisprop = get_property ("scriptHorizontal",0);
      if (gh_boolean_p (axisprop) && gh_scm2bool (axisprop))
	{
	  ss->axis_ = X_AXIS;
	  text->set_parent (ss, Y_AXIS);
			       
	}
      ss->set_victim (text);
      ss->set_elt_property (script_priority_scm_sym,
			    gh_int2scm (200));

      ss->dir_ = r->dir_;

      text->text_str_ = r->text_str_;
      
      if (r->style_str_.length_i ())
	text->set_elt_property (style_scm_sym, ly_ch_C_to_scm (r->style_str_.ch_C()));
      
      SCM empty = get_property ("textEmptyDimension", 0);
      if (gh_boolean_p (empty) && gh_scm2bool (empty))
	{
	  text->set_empty (true, X_AXIS);
	}

      announce_element (Score_element_info (text, r));
      announce_element (Score_element_info (ss, r));

      texts_.push (text);
      positionings_.push (ss);
    }
}

void
Text_engraver::do_pre_move_processing ()
{
  for (int i=0; i < texts_.size (); i++)
    {
      typeset_element (texts_[i]);
      typeset_element (positionings_[i]);
    }
  texts_.clear ();
  positionings_.clear ();
}

void
Text_engraver::do_post_move_processing ()
{
  reqs_.clear ();
}

ADD_THIS_TRANSLATOR(Text_engraver);
