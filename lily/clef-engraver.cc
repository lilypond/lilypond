/*
  clef.cc -- implement Clef_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>,
  Mats Bengtsson <matsb@s3.kth.se>
*/

#include <ctype.h>
#include "bar.hh"
#include "clef-engraver.hh"
#include "clef-item.hh"
#include "debug.hh"
#include "command-request.hh"
#include "time-description.hh"


#include "note-head.hh"
#include "key-item.hh"
#include "local-key-item.hh"

Clef_engraver::Clef_engraver()
{
  clef_p_ = 0;
  clef_req_l_ = 0;
  clef_type_str_ = "";
  c0_position_i_ = 0;
  clef_position_i_ = 0;
  octave_dir_ = CENTER;
}

/*
  Ugh.  Should have support for Dictionaries in mudela.
 */
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
  if (s == "treble" ||
      s == "violin" ||
      s == "G" || s == "G2")
    {
      clef_type_str_ = "violin";
      clef_position_i_ = -2;
    }
  else if (s == "french")
    {
      clef_type_str_ = "violin";
      clef_position_i_ = -4;
    }
  else if (s == "soprano")
    {
      clef_type_str_ = "alto";
      clef_position_i_ = -4;
    }
  else if (s == "mezzosoprano")
    {
      clef_type_str_ = "alto";
      clef_position_i_ = -2;
    }
  else if (s == "alto")
    {
      clef_type_str_ = "alto";
      clef_position_i_ = 0;
    }
  else if (s == "tenor")
    {
      clef_type_str_ = "alto";
      clef_position_i_ = 2;
    }
  else if (s == "baritone")
    {
      clef_type_str_ = "alto";
      clef_position_i_ = 4;
    }
  else if (s == "varbaritone")
    {
      clef_type_str_ = "bass";
      clef_position_i_ = 0;
    }
  else if (s == "bass" || s == "F")
    {
      clef_type_str_ = "bass";
      clef_position_i_ = 2;
    }
  else if (s == "subbass")
    {
      clef_type_str_ = "bass";
      clef_position_i_ = 4;
    }
  else 
    {
      switch(toupper (s[0]))
	{
	case 'F': 
	  clef_type_str_ = "bass";
	  break;
	case  'G':
	  clef_type_str_ = "violin";
	  break;
	case 'C': 
	  clef_type_str_ = "alto";
	  break;
	default:
	  return false;
	}
      clef_position_i_ = 2 * (s[1] - '0') - 6;
    }
  if (clef_type_str_ == "violin")
    c0_position_i_ = clef_position_i_ - 4;
  else if (clef_type_str_ == "alto")
    c0_position_i_ = clef_position_i_;
  else if (clef_type_str_ == "bass")
    c0_position_i_ = clef_position_i_ + 4;
  else
    assert (false);

      
  c0_position_i_ -= (int) octave_dir_ * 7;
  
  return true;
}

void
Clef_engraver::read_req (Clef_change_req*c_l)
{
  if (!set_type (c_l->clef_str_))
    c_l->error (_ ("unknown clef type "));
}


/** 
  Generate a clef at the start of a measure. (when you see a Bar,
  ie. a breakpoint) 
  */
void
Clef_engraver::acknowledge_element (Score_element_info info)
{
  if (info.elem_l_->is_type_b (Bar::static_name ()) 
      && clef_type_str_.length_i())
    {
      create_clef();
      if (!clef_req_l_)
	clef_p_->default_b_ = true;
    }

  /* ugh; should make Clef_referenced baseclass */
  Item * it_l =info.elem_l_->access_Item ();
if (it_l)
  {
  if (it_l->is_type_b (Note_head::static_name ()))
    {
      Note_head * h = (Note_head*)it_l;
      h->position_i_ += c0_position_i_;
    }
  else if (it_l->is_type_b (Local_key_item::static_name ()))
    {
      Local_key_item *i = (Local_key_item*)it_l;
      i->c0_position_i_ =c0_position_i_;
    }
  else if (it_l->is_type_b (Key_item::static_name ()))
    {
      Key_item *k = (Key_item*)it_l;
      k-> set_c_position (c0_position_i_);
    }
  } 
}

void
Clef_engraver::do_creation_processing()
{
  Scalar def = get_property ("defaultClef");
  if (def.to_bool ()) // egcs: Scalar to bool is ambiguous
    set_type (def);
  
  if (clef_type_str_.length_i ())
    { 
      create_clef();
      clef_p_->default_b_ = false;
    }
}

bool
Clef_engraver::do_try_request (Request * r_l)
{
  Command_req* creq_l= r_l->access_Command_req ();
  if (!creq_l || !creq_l->access_Clef_change_req ())
    return false;

  clef_req_l_ = creq_l->access_Clef_change_req ();
  read_req (clef_req_l_);
  return true;
}

void
Clef_engraver::create_clef()
{
  if (!clef_p_)
    {
      clef_p_ = new Clef_item;
      clef_p_->break_priority_i_ = -2; // ugh
      announce_element (Score_element_info (clef_p_,clef_req_l_));
    }
  clef_p_->read (*this);
}

void
Clef_engraver::do_process_requests()
{
  if (clef_req_l_)
    {
      create_clef();
      clef_p_->default_b_ = false;
    }
}

void
Clef_engraver::do_pre_move_processing()
{
  if (clef_p_)
    {
      typeset_element (clef_p_);
      clef_p_ = 0;
    }
}
void
Clef_engraver::do_post_move_processing()
{
  clef_req_l_ = 0;
}

void
Clef_engraver::do_removal_processing()
{
  if (clef_p_)
    {
      clef_p_->unlink ();	
      delete clef_p_;
      clef_p_ =0;
    }
}


IMPLEMENT_IS_TYPE_B1(Clef_engraver,Engraver);
ADD_THIS_TRANSLATOR(Clef_engraver);
