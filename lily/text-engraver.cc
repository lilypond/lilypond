/*   
  text-engraver.cc --  implement Text_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "g-staff-side.hh"
#include "g-text-item.hh"
#include "text-def.hh"
#include "note-head.hh"
#include "stem.hh"
#include "staff-sym.hh"

class Text_engraver : public Engraver
{
  Link_array<Script_req> reqs_;
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
  if (Script_req *r = dynamic_cast<Script_req*> (m))
    {
      Text_def * t = dynamic_cast<Text_def*> (r->scriptdef_p_);
      if (!t)
	return false;
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
      Script_req * r = reqs_[i];
      Text_def * t= dynamic_cast<Text_def*> (r->scriptdef_p_);

      G_text_item *text = new G_text_item;
      G_staff_side_item *ss = new G_staff_side_item;
      ss->set_victim (text);
      ss->dir_ = r->dir_;
      Scalar p (get_property ("textstyle", 0)); // textStyle?
      if (p.length_i ())
	text->style_str_ = p;
      text->text_str_ = t->text_str_;

      Scalar padding = get_property ("textScriptPadding", 0);
      if (padding.length_i() && padding.isnum_b ())
	{
	  ss->padding_f_ = Real(padding);
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
  Staff_symbol* s_l = get_staff_info().staff_sym_l_;
  for (int i=0; i < texts_.size (); i++)
    {
      if (s_l != 0)
	{
	  positionings_[i]->add_support (s_l);
	}

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
