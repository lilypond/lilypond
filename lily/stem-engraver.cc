/*
  stem-grav.cc -- implement Stem_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "staff-symbol-referencer.hh"
#include "stem-engraver.hh"
#include "note-head.hh"
#include "stem.hh"
#include "musical-request.hh"
#include "misc.hh"
#include "stem-tremolo.hh"
#include "staff-info.hh"
#include "translator-group.hh"

Stem_engraver::Stem_engraver()
{
  abbrev_req_l_ = 0;
  stem_p_ = 0;
  abbrev_p_ = 0;
  default_abbrev_i_ = 16;
  rhythmic_req_l_ =0;
}

void
Stem_engraver::do_creation_processing ()
{
  SCM prop = get_property ("abbrev");
  if (gh_number_p(prop)) 
    {
      default_abbrev_i_  = gh_scm2int (prop);
    }
}

void
Stem_engraver::acknowledge_element(Score_element_info i)
{
  if (Rhythmic_head * h = dynamic_cast<Rhythmic_head *> (i.elem_l_))
    {
      if (h->stem_l ())
	return;
      
      Rhythmic_req * r = dynamic_cast <Rhythmic_req *> (i.req_l_);
      int duration_log = r->duration_.durlog_i_;      
      if (!stem_p_) 
	{
	  stem_p_ = new Stem;
	  Staff_symbol_referencer_interface st(stem_p_);
	  st.set_interface ();
	  
	  stem_p_->set_elt_property ("duration-log", gh_int2scm (duration_log));

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
		  abbrev_p_ = new Stem_tremolo;
		  announce_element (Score_element_info (abbrev_p_, abbrev_req_l_));
		  abbrev_p_->set_elt_property ("tremolo-flags", gh_int2scm (intlog2 (t) - (duration_log>? 2)));
		}
	    }

	  // must give the request, to preserve the rhythmic info.
	  announce_element (Score_element_info (stem_p_, r));
	}

      if (stem_p_->flag_i () != duration_log)
	{
	  r->warning (_f ("Adding note head to incompatible stem (type = %d)", 1 <<  stem_p_->flag_i ()));
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
      SCM prop = get_property ("stemLeftBeamCount");
      if (gh_number_p(prop))
	{
	  stem_p_->set_beaming (gh_scm2int (prop),LEFT);
	  daddy_trans_l_->set_property ("stemLeftBeamCount", SCM_UNDEFINED);
	}
      prop = get_property ("stemRightBeamCount");
      if (gh_number_p(prop))
	{
	  stem_p_->set_beaming (gh_scm2int (prop), RIGHT);
	  daddy_trans_l_->set_property ("stemRightBeamCount", SCM_UNDEFINED);
	}

      // UGH. Should mark non-forced instead.
      SCM dir = stem_p_->get_elt_property ("direction");
      if (gh_number_p (dir) && to_dir(dir))
	{
	  stem_p_->set_elt_property ("dir-forced", SCM_BOOL_T);	  
	}


      typeset_element(stem_p_);
      stem_p_ = 0;
    }
  abbrev_req_l_ = 0;
}

bool
Stem_engraver::do_try_music (Music* r)
{
  if (Tremolo_req* a = dynamic_cast <Tremolo_req *> (r))
    {
      abbrev_req_l_ = a;
      return true;
    }
  return false;
}


ADD_THIS_TRANSLATOR(Stem_engraver);

