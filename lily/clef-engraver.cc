/*

  clef-engraver.cc -- implement Clef_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>,

  Mats Bengtsson <matsb@s3.kth.se>
*/

#include <ctype.h>

#include "translator-group.hh"
#include "key-item.hh"
#include "local-key-item.hh"
#include "bar.hh"
#include "note-head.hh"
#include "staff-symbol-referencer.hh"
#include "debug.hh"
#include "command-request.hh"
#include "engraver.hh"
#include "direction.hh"
#include "side-position-interface.hh"
#include "item.hh"

/// where is c-0 in the staff?
class Clef_engraver : public  Engraver
{
public:
  VIRTUAL_COPY_CONS (Translator);
  Clef_engraver ();

  Direction octave_dir_;
  bool  first_b_;

protected:
  virtual void do_process_music ();
  virtual void do_pre_move_processing ();
  virtual void do_creation_processing ();
  virtual void do_post_move_processing ();
  virtual bool do_try_music (Music*);
  virtual void acknowledge_element (Score_element_info);

private:
  Item * clef_p_;
  Item * octavate_p_;
  Clef_change_req * clef_req_l_;
  
  void create_clef ();
  bool set_type (String);
};


Clef_engraver::Clef_engraver ()
{
  first_b_ = true;
  clef_p_ = 0;
  clef_req_l_ = 0;
  octave_dir_ = CENTER;
  octavate_p_ = 0;
}

bool
Clef_engraver::set_type (String s)
{
  if (s.right_str (2) == "_8") // Down one octave
    {
      octave_dir_ = DOWN;
      s = s.left_str (s.length_i () - 2);
    }
  else if (s.right_str (2) == "^8") // Up one octave
    {
      octave_dir_ = UP;
      s = s.left_str (s.length_i () - 2);
    }
  else
    octave_dir_ = CENTER;

  SCM c = get_property ("supportedClefTypes");
  SCM p = get_property ("clefPitches");
  
  if (gh_list_p (c))
    {
      SCM found = scm_assoc (ly_str02scm (s.ch_C ()), c);
      if (found == SCM_BOOL_F)
	return false;
      
      SCM glyph = gh_cadr (found);
      SCM pos = gh_caddr (found);

      daddy_trans_l_->set_property ("glyph", glyph);
      daddy_trans_l_->set_property ("position", pos);

      found = scm_assoc (glyph, p);
      if (found == SCM_BOOL_F)
	return false;

      int c0_position = gh_scm2int (pos) + gh_scm2int (gh_cdr (found));
      daddy_trans_l_->set_property ("c0-position", gh_int2scm (c0_position));
    }

  int c0_position = gh_scm2int (get_property ("c0-position"));
  c0_position -= (int)octave_dir_ * 7;
  daddy_trans_l_->set_property ("c0-position", gh_int2scm (c0_position));


  SCM basic = ly_symbol2scm ("Clef");
  SCM c0 = ly_symbol2scm ("c0-position");
  SCM gl = ly_symbol2scm ("glyph");

  daddy_trans_l_->execute_single_pushpop_property (basic, gl, SCM_UNDEFINED);
  daddy_trans_l_->execute_single_pushpop_property (basic, c0, SCM_UNDEFINED);  
  daddy_trans_l_->execute_single_pushpop_property (basic, gl,
						   get_property ("glyph"));
  daddy_trans_l_->execute_single_pushpop_property (basic, c0,
						   get_property ("c0_position")
						   );
  return true;
}

/** 
  Generate a clef at the start of a measure. (when you see a Bar,
  ie. a breakpoint) 
  */
void
Clef_engraver::acknowledge_element (Score_element_info info)
{
  Item * item =dynamic_cast <Item *> (info.elem_l_);
  if (item)
    {
      if (Bar::has_interface (info.elem_l_)
	  && gh_string_p (get_property ("glyph")))
	create_clef ();
      

      if (Note_head::has_interface (item)
	  || Local_key_item::has_interface (item))
	{
	  int p = int (Staff_symbol_referencer::position_f (item))
	    + gh_scm2int (get_property ("c0-position"));
	  Staff_symbol_referencer::set_position (item, p);
	}
      else if (Key_item::has_interface (item))
	{
	  item->set_elt_property ("c0-position", get_property ("c0-position"));
	}
    } 
}

void
Clef_engraver::do_creation_processing ()
{
  daddy_trans_l_->set_property ("position", gh_int2scm (0));
  daddy_trans_l_->set_property ("glyph", SCM_EOL);
  daddy_trans_l_->set_property ("c0-position", gh_int2scm (0));

  SCM def = get_property ("defaultClef");
  if (gh_string_p (def))
    {
      set_type (ly_scm2string (def));
    }
}

bool
Clef_engraver::do_try_music (Music * r_l)
{
  if (Clef_change_req *cl = dynamic_cast <Clef_change_req *> (r_l))
    {
      clef_req_l_ = cl;
      if (!set_type (cl->clef_str_))
	cl->origin ()->warning (_ ("unknown clef type"));

      return true;
    }
  return false;
}

void
Clef_engraver::create_clef ()
{
  if (!clef_p_)
    {
      Item *c= new Item (get_property ("Clef"));
      announce_element (c, clef_req_l_);

      Staff_symbol_referencer::set_interface (c);
      
      clef_p_ = c;
    }
  Staff_symbol_referencer::set_position (clef_p_,
					 gh_scm2int (get_property ("position")
						     ));
  if (octave_dir_)
    {
      Item * g = new Item (get_property ("OctavateEight"));

      Side_position::add_support (g,clef_p_);      

      g->set_parent (clef_p_, Y_AXIS);
      g->set_parent (clef_p_, X_AXIS);

      g->set_elt_property ("direction", gh_int2scm (octave_dir_));
      octavate_p_ = g;
      announce_element (octavate_p_, clef_req_l_);
    }
}

void
Clef_engraver::do_process_music ()
{
  if (clef_req_l_ || first_b_)
    {
      create_clef ();
      clef_p_->set_elt_property ("non-default", SCM_BOOL_T);
    }
}

void
Clef_engraver::do_pre_move_processing ()
{
  if (clef_p_)
    {
      SCM vis = 0; 
      if (to_boolean (clef_p_->get_elt_property ("non-default")))
	{
	  vis = get_property ("explicitClefVisibility");
	}

      if (vis)
	{
	  clef_p_->set_elt_property ("visibility-lambda", vis);
	  if (octavate_p_)
	    octavate_p_->set_elt_property ("visibility-lambda", vis);
	}
      
      typeset_element (clef_p_);
      clef_p_ =0;

      if (octavate_p_)
	typeset_element (octavate_p_);

      octavate_p_ = 0;
    }

  first_b_ = 0;
}

void
Clef_engraver::do_post_move_processing ()
{
  clef_req_l_ = 0;
}

ADD_THIS_TRANSLATOR (Clef_engraver);

