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
  clef_req_l_ = 0;
  clef_type_str_ = "";
  c0_position_i_ = 0;
  clef_position_i_ = 0;
  octave_dir_ = CENTER;
}

struct Clef_settings {
  char const *name;
  char const *cleftype;
  int position;
} clef_settings[] = {
  {"treble", "treble", -2},
  {"violin", "treble", -2},
  {"G", "treble", -2},
  {"G2", "treble", -2},  
  {"scarlatti", "scarlatti", 0 },
  {"french", "treble",-4 },
  {"soprano", "alto",-4 },
  {"mezzosoprano", "alto",-2 },
  {"alto", "alto",0 },
  {"tenor", "alto",2 },
  {"baritone", "alto",4 },
  {"varbaritone", "bass",0 },
  {"bass" , "bass",2 },
  {"F", "bass", 2},
  {"subbass", "bass",4},
  {0,0,0}
};




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

  bool found = 0;
  for (Clef_settings *c = clef_settings; !found && c->name; c++)
    {
      if (c->name == s)
	{
	  clef_type_str_ = c->cleftype;
	  clef_position_i_ = c->position;
	  found = 1;
	}
      }
  if (!found)
    {
      switch(toupper (s[0]))
	{
	case 'F': 
	  clef_type_str_ = "bass";
	  break;
	case  'G':
	  clef_type_str_ = "treble";
	  break;
	case 'C': 
	  clef_type_str_ = "alto";
	  break;
	default:
	  return false;
	}
      clef_position_i_ = 2 * (s[1] - '0') - 6;
    }

  if (clef_type_str_ == "treble")
    c0_position_i_ = clef_position_i_ - 4;
  else if (clef_type_str_ == "alto")
    c0_position_i_ = clef_position_i_;
  else if (clef_type_str_ == "bass")
    c0_position_i_ = clef_position_i_ + 4;
  else if (clef_type_str_ == "scarlatti")
    c0_position_i_ = 0;
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
  if (dynamic_cast<Bar*>(info.elem_l_)
      && clef_type_str_.length_i())
    {
      create_clef();
      if (!clef_req_l_)
	for (int i=0; i < clef_p_arr_.size (); i++)
	  {
	    clef_p_arr_[i]->default_b_ = true;
	  }

    }

  /* ugh; should make Clef_referenced baseclass */
  Item * it_l =dynamic_cast <Item *> (info.elem_l_);
  if (it_l)
    {
      if (Note_head * h = dynamic_cast<Note_head*>(it_l))
	{
	  h->position_i_ += c0_position_i_;
	}
      else if (Local_key_item *i = dynamic_cast<Local_key_item*> (it_l))
	{
	  i->c0_position_i_ =c0_position_i_;
	}
      else if (Key_item *k = dynamic_cast<Key_item*>(it_l))
	{
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
	for (int i=0; i < clef_p_arr_.size (); i++)

	    clef_p_arr_[i]->default_b_ = false;
    }
}

bool
Clef_engraver::do_try_music (Music * r_l)
{
  if (Clef_change_req *cl = dynamic_cast <Clef_change_req *> (r_l))
    {
      clef_req_l_ = cl;
      read_req (clef_req_l_);
      return true;
    }
  else
    return false;

}

void
Clef_engraver::create_clef()
{
  if (clef_type_str_ == "scarlatti")
    {
      while (clef_p_arr_.size () < 2)
	{
	  Clef_item *ct= new Clef_item;
	  ct->break_priority_i_ =  -2; // UGH
	  announce_element (Score_element_info (ct, clef_req_l_));
	  clef_p_arr_.push (ct);
	}
      clef_p_arr_[0]->symbol_ = "treble";
      clef_p_arr_[0]->y_position_i_ = 4;
      clef_p_arr_[1]->symbol_ = "bass";
      clef_p_arr_[1]->y_position_i_ = -4;
    }
  else
    {
      if (!clef_p_arr_.size ())
	{
	  Clef_item *c= new Clef_item;
	  c->break_priority_i_ = -2; // ugh
	  announce_element (Score_element_info (c, clef_req_l_));
	  clef_p_arr_.push (c);
	}

      for (int i=0; i < clef_p_arr_.size (); i++)
	{
	  clef_p_arr_[i]->symbol_ = clef_type_str_;
	  clef_p_arr_[i]->y_position_i_ = clef_position_i_;
	  clef_p_arr_[i]->octave_dir_ = octave_dir_;
	}
    }
}


void
Clef_engraver::do_process_requests()
{
  if (clef_req_l_)
    {
      create_clef();
      for (int i=0; i < clef_p_arr_.size (); i++)
	clef_p_arr_[i]->default_b_ = false;
    }
}

void
Clef_engraver::do_pre_move_processing()
{
  for (int i=0; i <clef_p_arr_.size (); i++)
    {
      typeset_element (clef_p_arr_[i]);
    }
  clef_p_arr_.clear ();
}

void
Clef_engraver::do_post_move_processing()
{
  clef_req_l_ = 0;
}

void
Clef_engraver::do_removal_processing()
{
  if (clef_p_arr_.size ())
    {
      assert (false);
    }
}



ADD_THIS_TRANSLATOR(Clef_engraver);
