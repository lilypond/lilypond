/*   
  text-engraver.cc --  implement Text_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "g-staff-side.hh"
#include "g-text-item.hh"
#include "musical-request.hh"
#include "note-head.hh"
#include "stem.hh"
#include "staff-symbol.hh"

class Text_engraver : public Engraver
{
  Link_array<Text_script_req> reqs_;
  Link_array<G_staff_side_item> positionings_;
  Link_array<G_text_item> texts_;
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

      G_text_item *text = new G_text_item;
      G_staff_side_item *ss = new G_staff_side_item;

      ss->set_victim (text);
      ss->set_elt_property (script_priority_scm_sym,
			    gh_int2scm (200));

      ss->dir_ = r->dir_;

      text->text_str_ = r->text_str_;
      
      if (r->style_str_.empty_b ())
	{
	  Scalar p (get_property ("textStyle", 0));
	  if (p.length_i ())
	    text->style_str_ = p;
	}
      else
	text->style_str_ = r->style_str_;
      
      Scalar padding = get_property ("textScriptPadding", 0);
      if (padding.length_i() && padding.isnum_b ())
	{
	  ss->set_elt_property (padding_scm_sym, gh_double2scm(Real(padding)));
	}

      Scalar empty = get_property ("textEmptyDimension", 0);
      if (empty.to_bool ())
	{
	  text->dim_cache_[X_AXIS]->set_empty (true);
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
