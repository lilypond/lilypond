/*
  stem-grav.cc -- implement Stem_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "stem-engraver.hh"
#include "note-head.hh"
#include "stem.hh"
#include "musical-request.hh"
#include "duration-convert.hh"
#include "misc.hh"
#include "abbrev.hh"

Stem_engraver::Stem_engraver()
{
  abbrev_req_l_ = 0;
  stem_p_ = 0;
  abbrev_p_ = 0;
  default_abbrev_i_ = 16;
  dir_ = CENTER;
}

void
Stem_engraver::do_creation_processing ()
{
  Scalar prop = get_property ("abbrev");
  if (prop.isnum_b ()) 
    {
      default_abbrev_i_  = prop;
    }
}

void
Stem_engraver::acknowledge_element(Score_element_info i)
{
  if (dynamic_cast<Rhythmic_head *> (i.elem_l_))
    {
      Rhythmic_head *h  = dynamic_cast<Rhythmic_head *> (i.elem_l_);
      if (!stem_p_) 
	{
	  Rhythmic_req * r = dynamic_cast <Rhythmic_req *> (i.req_l_);
	  stem_p_ = new Stem;
	  int durlog_i = r->duration_.durlog_i_;
	  stem_p_->flag_i_ = durlog_i;

	  
	  if (abbrev_req_l_)
	    {
	      /*
		suggests typing of:
		   c8:16 c: c: c:
	        hmm, which isn't so bad?
	      */
	      int t = abbrev_req_l_->type_i_;
	      if (!t)
		t = default_abbrev_i_;
	      else
		default_abbrev_i_ = t;

	      if (t)
		{
		  abbrev_p_ = new Abbreviation;
		  announce_element (Score_element_info (abbrev_p_, abbrev_req_l_));
		  abbrev_p_->abbrev_flags_i_ =intlog2 (t) - (durlog_i>? 2);
		}
	    }

	  // must give the request, to preserve the rhythmic info.
	  announce_element (Score_element_info (stem_p_, r));
	}
      stem_p_->add_head (h);
    }
}

void
Stem_engraver::do_pre_move_processing()
{
  if (abbrev_p_)
    {
      abbrev_p_->set_stem (stem_p_);
      typeset_element (abbrev_p_);
      abbrev_p_ = 0;
    }
  if (stem_p_)
    {
      Scalar prop = get_property ("ydirection");
      dir_ = prop.isnum_b () ? (Direction)int(prop) : CENTER;
      if (dir_)
	{
	  stem_p_->dir_ = dir_;
	  stem_p_->dir_forced_b_ = true;
	}

      typeset_element(stem_p_);
      stem_p_ = 0;
    }
  abbrev_req_l_ = 0;
}

bool
Stem_engraver::do_try_request (Request* r)
{
  Musical_req* mus_l = dynamic_cast <Musical_req *> (r);
  if (!mus_l)
    return false;
  
  Abbreviation_req* a = dynamic_cast <Abbreviation_req *> (mus_l);
  if (!a)
    return false;

  abbrev_req_l_ = a;

  return true;
}

IMPLEMENT_IS_TYPE_B1(Stem_engraver, Engraver);
ADD_THIS_TRANSLATOR(Stem_engraver);
