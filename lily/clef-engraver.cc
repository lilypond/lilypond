/*
  clef.cc -- implement Clef_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>,

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
  create_default_b_ = true;
}

/*
  PUT THIS IN GUILE!
 */
struct Clef_settings {
  char const *name;
  char const *cleftype;
  int position;
} clef_settings[] = {
  {"treble", "treble", -2},
  {"violin", "treble", -2},
  {"G", "treble", -2},
  {"G2", "treble", -2},  
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
  else
    assert (false);
      
  c0_position_i_ -= (int) octave_dir_ * 7;
  
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
      && clef_type_str_.length_i())
    {
      bool def = !clef_p_;
      create_clef();
      if(def)
	clef_p_->set_elt_property(visibility_lambda_scm_sym,
				  ly_ch_C_eval_scm ("postbreak_only_visibility"));
    }

  /* ugh; should make Clef_referenced baseclass */
  Item * it_l =dynamic_cast <Item *> (info.elem_l_);
  if (it_l)
    {
      if (Note_head * h = dynamic_cast<Note_head*>(it_l))
	{
	  //	  h->position_i_ += c0_position_i_;
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
  create_default_b_ = true;	// should read property.
  SCM def = get_property ("createInitdefaultClef", 0);
  if (gh_string_p (def))
    set_type (ly_scm2string (def));
  
  if (clef_type_str_.length_i ())
    { 
      create_clef();
      clef_p_->set_elt_property (non_default_scm_sym, SCM_BOOL_T);
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
      Clef_item *c= new Clef_item;
      c->set_elt_property (break_priority_scm_sym, gh_int2scm (-2)); // ugh
      SCM clefstyle = get_property ("clefStyle", 0);
      if (gh_string_p(clefstyle))
	c->set_elt_property (style_scm_sym, clefstyle);
      
      announce_element (Score_element_info (c, clef_req_l_));
      clef_p_ = c;
    }
  
  clef_p_->symbol_ = clef_type_str_;
  clef_p_->y_position_i_ = clef_position_i_;
  if (octave_dir_)
    {
      clef_p_->set_elt_property (octave_dir_scm_sym, gh_int2scm (octave_dir_));
    }
}


void
Clef_engraver::do_process_requests()
{
  if (clef_req_l_)
    {
      create_clef();
    }
  else if (create_default_b_)
    {
      SCM type = get_property ("defaultClef", 0);
      if (gh_string_p (type))
	set_type (ly_scm2string (type));
      else
	set_type ( "treble");
      create_clef ();
      create_default_b_ =0;
    }
}

void
Clef_engraver::do_pre_move_processing()
{
  if (clef_p_)
    {
      typeset_element (clef_p_);
      clef_p_ =0;
    }
  create_default_b_ = false;
}

void
Clef_engraver::do_post_move_processing()
{
  clef_req_l_ = 0;
}

void
Clef_engraver::do_removal_processing()
{
  assert (!clef_p_);
}



ADD_THIS_TRANSLATOR(Clef_engraver);
