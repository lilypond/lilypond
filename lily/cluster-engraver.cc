/*
  cluster-engraver.cc -- implement Cluster_engraver

  (c) 2002 Juergen Reuter <reuter@ipd.uka.de>
*/

#include "engraver.hh"
#include "item.hh"
#include "spanner.hh"
#include "note-head.hh"
#include "protected-scm.hh"
#include "warn.hh"

class Cluster_engraver : public Engraver
{

protected:
TRANSLATOR_DECLARATIONS(Cluster_engraver);
  virtual void start_translation_timestep ();
  virtual bool try_music (Music *);
  virtual void process_music ();  
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();

private:
  Drul_array<Music*> reqs_drul_;
  Pitch pitch_min_, pitch_max_;
  Spanner *cluster_;
  Protected_scm columns_scm_;
};

void reset_min_max (Pitch *pitch_min, Pitch *pitch_max)
{
  /*
   * (pitch_min > pitch_max) means that pitches are not yet
   * initialized
   */
  *pitch_min = Pitch (0, 0, +1);
  *pitch_max = Pitch (0, 0, -1);
}

Cluster_engraver::Cluster_engraver ()
{
  cluster_ = 0;
  columns_scm_ = SCM_EOL;
  reqs_drul_[LEFT] = reqs_drul_[RIGHT] = 0;
}

bool
Cluster_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("abort-event"))
    {
      reqs_drul_[START] = 0;
      reqs_drul_[STOP] = 0;
      if (cluster_)
	{
	  cluster_->suicide ();
	  cluster_ = 0;
	  columns_scm_ = SCM_EOL;
	}
    }
  else if (m->is_mus_type ("cluster-event"))
    {
      Direction d = to_dir (m->get_mus_property ("span-direction"));
      reqs_drul_[d] = m;
      return true;
    }
  return false;
}

void
Cluster_engraver::process_music ()
{
  if (reqs_drul_[STOP])
    {
      if (!cluster_)
	{
	  reqs_drul_[STOP]->origin ()->warning ("can't find start of cluster");
	}
      else
	{
	  Grob *bound = unsmob_grob (get_property ("currentMusicalColumn"));
	  cluster_->set_bound (RIGHT, bound);
	}
    }
  if (reqs_drul_[START])
    {
      if (cluster_)
	{
	  reqs_drul_[START]->origin ()->warning ("may not nest clusters");
	}
      else
	{
	  SCM basicProperties = get_property ("Cluster");
	  cluster_ = new Spanner (basicProperties);
	  columns_scm_ = SCM_EOL;
	  Grob *bound = unsmob_grob (get_property ("currentMusicalColumn"));
	  cluster_->set_bound (LEFT, bound);
	  announce_grob (cluster_, bound->self_scm ());
	}
      reqs_drul_[START] = 0;
    }
}

void
Cluster_engraver::start_translation_timestep ()
{
  reset_min_max (&pitch_min_, &pitch_max_);
}

void
Cluster_engraver::stop_translation_timestep ()
{
  if (cluster_)
    {
      SCM column_scm = get_property ("currentMusicalColumn");
      if (column_scm == SCM_EOL)
	{
	  programming_error("failed retrieving current column");
	}
      else
	{
	  if (Pitch::compare (pitch_min_, pitch_max_) <= 0)
	    {
	      Real y_bottom = 0.5 * pitch_min_.steps ();
	      SCM c0 = get_property ("centralCPosition");
	      if (gh_number_p (c0))
		y_bottom += 0.5 * gh_scm2int (c0);
	      else
		programming_error ("Cluster_engraver: failed evaluating c0");
	      Real y_top = y_bottom +
		0.5 * (pitch_max_.steps () - pitch_min_.steps ());
	      column_scm = Protected_scm (column_scm);
	      SCM segment = scm_list_n (column_scm,
					gh_double2scm (y_bottom),
					gh_double2scm (y_top),
					SCM_UNDEFINED);
	      segment = scm_list_n (segment, SCM_UNDEFINED);
	      columns_scm_ = (columns_scm_ != SCM_EOL) ?
		gh_append2 (columns_scm_, segment) : segment;
	    }
	  else
	    {
	      /* This timestep is caused by a different voice of the
		 same staff and hence should be ignored. */
	    }
	}

      if (reqs_drul_[STOP])
	{
	  reqs_drul_[STOP] = 0;
	  cluster_->set_grob_property ("segments", columns_scm_);
	  typeset_grob (cluster_);
	  cluster_ = 0;
	  columns_scm_ = SCM_EOL;
	}
    }
}

void
Cluster_engraver::acknowledge_grob (Grob_info info)
{
  Item *item = dynamic_cast <Item *>(info.grob_);
  if (item)
    {
      if (Note_head::has_interface (info.grob_))
	{
	  Music *nr = info.music_cause ();
	  if (nr && nr->is_mus_type ("note-event"))
	    {
	      Pitch pitch = *unsmob_pitch (nr->get_mus_property ("pitch"));
	      if (Pitch::compare (pitch_min_, pitch_max_) > 0) // already init'd?
		{
		  // not yet init'd; use current pitch to init min/max
		  pitch_min_ = pitch;
		  pitch_max_ = pitch;
		}
	      else if (Pitch::compare (pitch, pitch_max_) > 0) // new max?
		{
		  pitch_max_ = pitch;
		}
	      else if (Pitch::compare (pitch, pitch_min_) < 0) // new min?
		{
		  pitch_min_ = pitch;
		}
	    }
	}
    }
}

ENTER_DESCRIPTION(Cluster_engraver,
/* descr */	"engraves a cluster",
/* creats*/	"Cluster",
/* accepts */	"cluster-event",
/* acks  */	"note-head-interface",
/* reads */	"",
/* write */	"");
