/*   
  ligature-bracket-engraver.cc -- implement Ligature__bracket_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2002 Juergen Reuter <reuter@ipd.uka.de>
  
 */
#include "engraver.hh"
#include "musical-request.hh"
#include "warn.hh"
#include "drul-array.hh"
#include "item.hh"
#include "spanner.hh"
#include "score-engraver.hh"
#include "note-head.hh"
#include "stem.hh"
#include "rest.hh"

class Ligature_bracket_engraver : public Engraver
{
  Drul_array<Span_req*> reqs_drul_;
  
  Spanner *finished_ligature_bracket_p_;
  Spanner *ligature_bracket_p_;
  Span_req *prev_start_req_;

  // moment where ligature started.
  Moment ligature_start_mom_;
  Grob *last_bound;

protected:
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void finalize ();

  virtual void acknowledge_grob (Grob_info);
  virtual bool try_music (Music*);
  virtual void process_music ();

public:
  TRANSLATOR_DECLARATIONS(Ligature_bracket_engraver);

private:
  void typeset_ligature_bracket ();
};


Ligature_bracket_engraver::Ligature_bracket_engraver ()
{
  ligature_bracket_p_ = 0;
  finished_ligature_bracket_p_ = 0;
  reqs_drul_[LEFT] = reqs_drul_[RIGHT] = 0;
  prev_start_req_ = 0;
  last_bound = 0;
}

bool
Ligature_bracket_engraver::try_music (Music *m)
{
  if (Span_req *req_ = dynamic_cast<Span_req*> (m))
    {
      if (scm_equal_p (req_->get_mus_property ("span-type"),
		       ly_str02scm ("abort")) == SCM_BOOL_T)
	{
	  reqs_drul_[START] = 0;
	  reqs_drul_[STOP] = 0;
	  if (ligature_bracket_p_)
	    ligature_bracket_p_->suicide ();
	  ligature_bracket_p_ = 0;
	}
      else if (scm_equal_p (req_->get_mus_property ("span-type"),
			    ly_str02scm ("ligature-bracket")) == SCM_BOOL_T)
	{
	  Direction d = req_->get_span_dir ();
	  reqs_drul_[d] = req_;
	  return true;
	}
    }
  return false;
}

void
Ligature_bracket_engraver::process_music ()
{
  if (reqs_drul_[STOP])
    {
      if (!ligature_bracket_p_)
	reqs_drul_[STOP]->origin ()->warning (_ ("can't find start of ligature"));
      else
	{
	  if (!last_bound)
	    {
	      reqs_drul_[STOP]->origin ()->warning (_ ("no right bound"));
	    }
	  else
	    {
	      ligature_bracket_p_->set_bound (RIGHT, last_bound);
	    }
	}
      prev_start_req_ = 0;
      finished_ligature_bracket_p_ = ligature_bracket_p_;
      ligature_bracket_p_ = 0;
    }
  last_bound = unsmob_grob (get_property ("currentMusicalColumn"));

  if (ligature_bracket_p_)
    {
      // TODO: maybe forbid breaks only if not transcribing
      top_engraver ()->forbid_breaks ();
    }
  if (reqs_drul_[START])
    {
      if (ligature_bracket_p_)
	{
	  reqs_drul_[START]->origin ()->warning (_ ("already have a ligature"));
	  return;
	}

      prev_start_req_ = reqs_drul_[START];
      ligature_bracket_p_ = new Spanner (get_property ("LigatureBracket"));

      Grob *bound = unsmob_grob (get_property ("currentMusicalColumn"));
      if (!bound)
	{
	  reqs_drul_[START]->origin ()->warning (_ ("no left bound"));
	}
      else
	{
	  ligature_bracket_p_->set_bound (LEFT, bound);
	}

      ligature_start_mom_ = now_mom ();
      
      announce_grob(ligature_bracket_p_, reqs_drul_[START]->self_scm());
    }
}

void
Ligature_bracket_engraver::start_translation_timestep ()
{
  reqs_drul_[START] = 0;
  reqs_drul_[STOP] = 0;
}

void
Ligature_bracket_engraver::typeset_ligature_bracket ()
{
  if (finished_ligature_bracket_p_)
    {
      typeset_grob (finished_ligature_bracket_p_);
      finished_ligature_bracket_p_ = 0;
    }
}

void
Ligature_bracket_engraver::stop_translation_timestep ()
{
  typeset_ligature_bracket ();
}

void
Ligature_bracket_engraver::finalize ()
{
  typeset_ligature_bracket ();
  if (ligature_bracket_p_)
    {
      prev_start_req_->origin ()->warning (_ ("unterminated ligature"));
      ligature_bracket_p_->suicide ();
    }
}

void
Ligature_bracket_engraver::acknowledge_grob (Grob_info info)
{
  if (ligature_bracket_p_)
    {
      if (Rest::has_interface (info.grob_l_))
	{
	  reqs_drul_[START]->origin ()->warning (_ ("ligature may not contain rest; ignoring rest"));
	  prev_start_req_->origin ()->warning (_ ("ligature was started here"));
	  // TODO: maybe better should stop ligature here rather than
	  // ignoring the rest?
	}
    }
}

ENTER_DESCRIPTION(Ligature_bracket_engraver,
/* descr */       "Handles Ligature_requests by engraving Ligature brackets.",
/* creats*/       "LigatureBracket",
/* acks  */       "rest-interface",
/* reads */       "",
/* write */       "");
