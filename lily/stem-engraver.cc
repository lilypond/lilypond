/*
  stem-engraver.cc -- implement Stem_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "context.hh"
#include "directional-element-interface.hh"
#include "engraver.hh"
#include "item.hh"
#include "misc.hh"
#include "rhythmic-head.hh"
#include "script-interface.hh"
#include "staff-symbol-referencer.hh"
#include "stem-tremolo.hh"
#include "stem.hh"

/**
   Make stems upon receiving noteheads.
*/
class Stem_engraver : public Engraver
{
  Grob *stem_;
  Grob *tremolo_;
  Music *rhythmic_ev_;
  Music *tremolo_ev_;

  TRANSLATOR_DECLARATIONS (Stem_engraver);

protected:
  void make_stem (Grob_info);

  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();
  virtual bool try_music (Music *);
};

Stem_engraver::Stem_engraver ()
{
  tremolo_ev_ = 0;
  stem_ = 0;
  tremolo_ = 0;
  rhythmic_ev_ = 0;
}

void
Stem_engraver::make_stem (Grob_info gi)
{
  /* Announce the cause of the head as cause of the stem.  The
     stem needs a rhythmic structure to fit it into a beam.  */
  stem_ = make_item ("Stem", gi.music_cause ()->self_scm ());

  /*
    docme: why do we take duration-log from request, not from note
    head?
  */
  int duration_log = gi.music_cause ()->duration_log ();
  stem_->set_property ("duration-log", scm_int2num (duration_log));

  if (tremolo_ev_)
    {
      /* Stem tremolo is never applied to a note by default,
	 is must me requested.  But there is a default for the
	 tremolo value:

	 c4:8 c c:

	 the first and last (quarter) note bothe get one tremolo flag.  */
      int requested_type
	= scm_to_int (tremolo_ev_->get_property ("tremolo-type"));
      SCM f = get_property ("tremoloFlags");
      if (!requested_type)
	{
	  if (scm_is_number (f))
	    requested_type = scm_to_int (f);
	  else
	    requested_type = 8;
	}
      else
	context ()->set_property ("tremoloFlags", scm_int2num (requested_type));

      int tremolo_flags = intlog2 (requested_type) - 2
	- (duration_log > 2 ? duration_log - 2 : 0);
      if (tremolo_flags <= 0)
	{
	  tremolo_ev_->origin ()->warning (_ ("tremolo duration is too long"));
	  tremolo_flags = 0;
	}

      if (tremolo_flags)
	{
	  tremolo_ = make_item ("StemTremolo", tremolo_ev_->self_scm ());

	  /* The number of tremolo flags is the number of flags of the
	     tremolo-type minus the number of flags of the note itself.  */
	  tremolo_->set_property ("flag-count", scm_int2num (tremolo_flags));
	  tremolo_->set_parent (stem_, X_AXIS);
	  stem_->set_property ("tremolo-flag", tremolo_->self_scm ());
	  tremolo_->set_property ("stem", stem_->self_scm ());
	}
    }
}

void
Stem_engraver::acknowledge_grob (Grob_info gi)
{
  if (Rhythmic_head::has_interface (gi.grob_))
    {
      if (Rhythmic_head::get_stem (gi.grob_))
	return;

      Music *cause = gi.music_cause ();
      if (!cause)
	return;

      if (!stem_)
	make_stem (gi);

      int duration_log = cause->duration_log ();
      if (Stem::duration_log (stem_) != duration_log)
	{
	  // FIXME: 
	  gi.music_cause ()->origin ()->warning (_f ("adding note head to incompatible stem (type = %d)", 1 << Stem::duration_log (stem_)));
	  gi.music_cause ()->origin ()->warning (_f ("maybe input should specify polyphonic voices"));
	}

      Stem::add_head (stem_, gi.grob_);
    }
}

void
Stem_engraver::stop_translation_timestep ()
{
  tremolo_ = 0;
  if (stem_)
    {
      /* FIXME: junk these properties.  */
      SCM prop = get_property ("stemLeftBeamCount");
      if (scm_is_number (prop))
	{
	  Stem::set_beaming (stem_, scm_to_int (prop), LEFT);
	  context ()->unset_property (ly_symbol2scm ("stemLeftBeamCount"));
	}
      prop = get_property ("stemRightBeamCount");
      if (scm_is_number (prop))
	{
	  Stem::set_beaming (stem_, scm_to_int (prop), RIGHT);
	  context ()->unset_property (ly_symbol2scm ("stemRightBeamCount"));
	}
      stem_ = 0;
    }
  tremolo_ev_ = 0;
}

bool
Stem_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("tremolo-event"))
    {
      tremolo_ev_ = m;
      return true;
    }
  return false;
}

ADD_TRANSLATOR (Stem_engraver,
		/* descr */ "Create stems and single-stem tremolos.  It also works together with "
		"the beam engraver for overriding beaming.",
		/* creats*/ "Stem StemTremolo",
		/* accepts */ "tremolo-event",
		/* acks  */ "rhythmic-head-interface",
		/* reads */ "tremoloFlags stemLeftBeamCount stemRightBeamCount",
		/* write */ "");
