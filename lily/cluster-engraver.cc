/*
  cluster-engraver.cc -- implement Cluster_engraver

  (c) 2002--2003 Juergen Reuter <reuter@ipd.uka.de>

  Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "engraver.hh"
#include "item.hh"
#include "spanner.hh"
#include "note-head.hh"
#include "note-column.hh"
#include "group-interface.hh"

class Cluster_engraver : public Engraver
{

protected:
TRANSLATOR_DECLARATIONS(Cluster_engraver);
  virtual bool try_music (Music *);
  virtual void process_music ();  
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();

private:
  Drul_array<Music*> reqs_drul_;

  Spanner *cluster_;
};

Cluster_engraver::Cluster_engraver ()
{
  cluster_ = 0;
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
	  cluster_ = new Spanner (get_property ("Cluster"));
	  Grob *bound = unsmob_grob (get_property ("currentMusicalColumn"));
	  cluster_->set_bound (LEFT, bound);
	  announce_grob (cluster_, reqs_drul_[START]->self_scm ());
	}
      reqs_drul_[START] = 0;
    }
}


void
Cluster_engraver::stop_translation_timestep ()
{
  if (reqs_drul_[STOP])
    {
      reqs_drul_[STOP] = 0;
      typeset_grob (cluster_);
      cluster_ = 0;
    }
}

void
Cluster_engraver::acknowledge_grob (Grob_info info)
{
  if (cluster_ && Note_column::has_interface (info.grob_))
    {
      Pointer_group_interface::add_grob (cluster_, ly_symbol2scm ("columns"), info.grob_);
    }
}

ENTER_DESCRIPTION(Cluster_engraver,
/* descr */	"engraves a cluster",
/* creats*/	"Cluster",
/* accepts */	"cluster-event",
/* acks  */	"note-column-interface",
/* reads */	"",
/* write */	"");
