/*
  clef-engraver.cc -- implement Clef_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>,

  Mats Bengtsson <matsb@s3.kth.se>
*/

#include <ctype.h>

#include "staff-symbol-referencer.hh"
#include "bar.hh"
#include "clef-item.hh"
#include "debug.hh"
#include "command-request.hh"
#include "timing-translator.hh"
#include "note-head.hh"
#include "key-item.hh"
#include "local-key-item.hh"
#include "array.hh"
#include "engraver.hh"
#include "direction.hh"
#include "side-position-interface.hh"
#include "text-item.hh"

/// where is c-0 in the staff?
class Clef_engraver : public  Engraver {
  Clef_item * clef_p_;
  Text_item * octavate_p_;
  Clef_change_req * clef_req_l_;
  
  void create_clef();
  bool set_type (String);
protected:
  virtual void do_process_music();
  virtual void do_pre_move_processing();
  virtual void do_creation_processing();
  virtual void do_post_move_processing();
  virtual bool do_try_music (Music*);
  virtual void acknowledge_element (Score_element_info);
public:
  VIRTUAL_COPY_CONS(Translator);
  int c0_position_i_;
  int clef_position_i_;
				// junkme.
  Direction octave_dir_;
  SCM clef_glyph_;		// no need for protection. Always referenced somewhere else.
   
  Clef_engraver();

  SCM basic_properties_;
  Protected_scm current_settings_;
};


Clef_engraver::Clef_engraver()
{
  current_settings_ = SCM_EOL;
  basic_properties_ = SCM_EOL;
  
  clef_glyph_ = SCM_EOL;
  clef_p_ = 0;
  clef_req_l_ = 0;
  c0_position_i_ = 0;
  clef_position_i_ = 0;
  octave_dir_ = CENTER;
  octavate_p_ = 0;
}

bool
Clef_engraver::set_type (String s)
{
  if (s.right_str(2) == "_8") // Down one octave
    {
      octave_dir_ = DOWN;
      s = s.left_str(s.length_i() - 2);
    }
  else if (s.right_str(2) == "^8") // Up one octave
    {
      octave_dir_ = UP;
      s = s.left_str(s.length_i() - 2);
    }
  else
    octave_dir_ = CENTER;

  SCM c = get_property ("supportedClefTypes");
  SCM p = get_property ("clefPitches");
  
  if (gh_list_p (c))
    {
      SCM found = scm_assoc (ly_str02scm (s.ch_C()), c);
      if (found == SCM_BOOL_F)
	return false;
      
      clef_glyph_  = gh_cadr (found);
      SCM pos  = gh_caddr (found);

      clef_position_i_ = gh_scm2int (pos);

      found = scm_assoc (clef_glyph_, p);
      if (found == SCM_BOOL_F)
	return false;

      c0_position_i_ = clef_position_i_ + gh_scm2int (gh_cdr (found));
    }

  c0_position_i_ -= (int) octave_dir_ * 7;


  current_settings_ = gh_cons (gh_cons (ly_symbol2scm ("glyph"), clef_glyph_), basic_properties_);
  current_settings_ =
    gh_cons (gh_cons (ly_symbol2scm ("c0-position"),
		      gh_int2scm (c0_position_i_)),
	     current_settings_);
  
  return true;
}


/** 
  Generate a clef at the start of a measure. (when you see a Bar,
  ie. a breakpoint) 
  */
void
Clef_engraver::acknowledge_element (Score_element_info info)
{
  if (dynamic_cast<Bar*>(info.elem_l_)
      && gh_string_p (clef_glyph_))
    create_clef();

  /* ugh; should make Clef_referenced baseclass */
  Item * it_l =dynamic_cast <Item *> (info.elem_l_);
  if (it_l)
    {
      if (dynamic_cast<Note_head*>(it_l)
	  || dynamic_cast<Local_key_item*> (it_l)
	  )
	  
	{
	  Staff_symbol_referencer_interface si (it_l);
	  si.set_position (int (si.position_f ()) + c0_position_i_);
	}
      else if (Key_item *k = dynamic_cast<Key_item*>(it_l))
	{
	  k->set_elt_property ("c0-position", gh_int2scm (c0_position_i_));
	}
    } 
}

void
Clef_engraver::do_creation_processing()
{
  basic_properties_ = get_property ("basicClefItemProperties");
  
  SCM def = get_property ("defaultClef");
  if (gh_string_p (def))
    {
      set_type (ly_scm2string (def));
      create_clef ();
      clef_p_->set_elt_property ("non-default", SCM_BOOL_T);
    }
}

bool
Clef_engraver::do_try_music (Music * r_l)
{
  if (Clef_change_req *cl = dynamic_cast <Clef_change_req *> (r_l))
    {
      clef_req_l_ = cl;
      if (!set_type (cl->clef_str_))
	cl->error (_ ("unknown clef type"));

      return true;
    }
  else
    return false;

}

void
Clef_engraver::create_clef()
{
  if (!clef_p_)
    {
      Clef_item *c= new Clef_item ( current_settings_);
      announce_element (Score_element_info (c, clef_req_l_));

      Staff_symbol_referencer_interface si(c);
      si.set_interface ();
      
      clef_p_ = c;
    }
  Staff_symbol_referencer_interface si(clef_p_);
  clef_p_->set_elt_property ("glyph", clef_glyph_);
  si.set_position (clef_position_i_);
  if (octave_dir_)
    {
      Text_item * g = new Text_item (get_property ("basicOctavateEightProperties"));
      Side_position_interface spi (g);
      spi.set_axis (Y_AXIS);
      spi.add_support (clef_p_);
      g->set_parent (clef_p_, Y_AXIS);
      g->set_parent (clef_p_, X_AXIS);
      g->add_offset_callback (Side_position_interface::aligned_on_self, X_AXIS);
      g->add_offset_callback (Side_position_interface::centered_on_parent, X_AXIS);
      g->set_elt_property ("direction", gh_int2scm (octave_dir_));
      octavate_p_ = g;
      announce_element (Score_element_info (octavate_p_, clef_req_l_));
    }
}


void
Clef_engraver::do_process_music()
{
  if (clef_req_l_)
    {
      create_clef();
      clef_p_->set_elt_property ("non-default", SCM_BOOL_T);
    }
}

void
Clef_engraver::do_pre_move_processing()
{
  if (clef_p_)
    {
      SCM vis = 0; 
      if(to_boolean (clef_p_->get_elt_property("non-default")))
	{
	  vis = ly_symbol2scm ("all-visible");
	  vis = scm_eval (vis);
	}

      if (vis)
	{
	  clef_p_->set_elt_property("visibility-lambda", vis);
	  if (octavate_p_)
	    octavate_p_->set_elt_property("visibility-lambda", vis);
	}
      
      typeset_element (clef_p_);
      clef_p_ =0;

      if (octavate_p_)
	typeset_element(octavate_p_);

      octavate_p_ = 0;
    }
}

void
Clef_engraver::do_post_move_processing()
{
  clef_req_l_ = 0;
}

ADD_THIS_TRANSLATOR(Clef_engraver);

