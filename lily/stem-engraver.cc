/*
  stem-grav.cc -- implement Stem_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "staff-symbol-referencer.hh"
#include "note-head.hh"
#include "stem.hh"
#include "musical-request.hh"
#include "misc.hh"
#include "stem-tremolo.hh"
#include "staff-info.hh"
#include "translator-group.hh"
#include "engraver.hh"

/**
  Make stems upon receiving noteheads.
 */
class Stem_engraver : public Engraver
{

public:
  VIRTUAL_COPY_CONS (Translator);
  Stem_engraver();
  
protected:
  virtual void do_creation_processing ();
  virtual void acknowledge_element (Score_element_info);
  virtual void do_pre_move_processing ();
  virtual bool do_try_music (Music*);
  
private:
  int default_tremolo_type_i_;
  Stem *stem_p_;
  Stem_tremolo *tremolo_p_;
  Rhythmic_req *rhythmic_req_l_;
  Tremolo_req* tremolo_req_l_;
};

ADD_THIS_TRANSLATOR (Stem_engraver);

Stem_engraver::Stem_engraver ()
{
  tremolo_req_l_ = 0;
  stem_p_ = 0;
  tremolo_p_ = 0;
  default_tremolo_type_i_ = 16;
  rhythmic_req_l_ =0;
}

void
Stem_engraver::do_creation_processing ()
{
  /*
    huh, why only at creation time?
  */
  SCM prop = get_property ("tremoloFlags");
  if (gh_number_p(prop)) 
    {
      default_tremolo_type_i_  = gh_scm2int (prop);
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
	  stem_p_ = new Stem (SCM_EOL);
	  Staff_symbol_referencer_interface st(stem_p_);
	  st.set_interface ();
	  
	  stem_p_->set_elt_property ("duration-log", gh_int2scm (duration_log));

	  if (tremolo_req_l_)
	    {
	      /*
		Stem tremolo is never applied to a note by default,
		is must me requested.  But there is a default for the
		tremolo value:

		   c4:8 c c:

		the first and last (quarter) note bothe get one tremolo flag.
	       */
	      int requested_type = tremolo_req_l_->type_i_;
	      if (!requested_type)
		requested_type = default_tremolo_type_i_;
	      else
		default_tremolo_type_i_ = requested_type;

	      if (requested_type)
		{
		  tremolo_p_ = new Stem_tremolo (get_property ("basicStemTremoloProperties"));
		  announce_element (Score_element_info (tremolo_p_, tremolo_req_l_));
		  /*
		    The number of tremolo flags is the number of flags of
		    the tremolo-type minus the number of flags of the note
		    itself.
		   */
		  int tremolo_flags = intlog2 (requested_type) - 2
		    - (duration_log > 2 ? duration_log - 2 : 0);
		  if (tremolo_flags < 0)
		    tremolo_flags = 0;
		  tremolo_p_->set_elt_property ("tremolo-flags",
						gh_int2scm (tremolo_flags));
		}
	    }
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
  if (tremolo_p_)
    {
      tremolo_p_->set_stem (stem_p_);
      typeset_element (tremolo_p_);
      tremolo_p_ = 0;
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

      /*
	 aargh: I don't get it.  direction is being set (and then set
	 to forced), if we have a Chord_tremolo.
       */
      SCM dir = stem_p_->get_elt_property ("direction");
      if (gh_number_p (dir) && to_dir(dir))
	{
	  stem_p_->set_elt_property ("dir-forced", SCM_BOOL_T);	  
	}

      typeset_element(stem_p_);
      stem_p_ = 0;
    }


  tremolo_req_l_ = 0;
}

bool
Stem_engraver::do_try_music (Music* r)
{
  if (Tremolo_req* a = dynamic_cast <Tremolo_req *> (r))
    {
      tremolo_req_l_ = a;
      return true;
    }
  return false;
}

