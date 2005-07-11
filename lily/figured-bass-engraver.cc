/*
  figured-bass-engraver.cc -- implement Figured_bass_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2002--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "engraver.hh"
#include "text-interface.hh"
#include "item.hh"
#include "context.hh"

class Figured_bass_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Figured_bass_engraver);
protected:
  Link_array<Music> figures_;
  Music *rest_event_;

  Grob *figure_;

  virtual bool try_music (Music *);
  virtual void stop_translation_timestep ();
  virtual void process_music ();
};

Figured_bass_engraver::Figured_bass_engraver ()
{
  figure_ = 0;
  rest_event_ = 0;
}

void
Figured_bass_engraver::stop_translation_timestep ()
{
  figure_ = 0;

  figures_.clear ();
  rest_event_ = 0;
}

bool
Figured_bass_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("bass-figure-event"))
    {
      figures_.push (m);
      return true;
    }
  else if (m->is_mus_type ("rest-event"))
    {
      rest_event_ = m;
      return true;
    }
  return false;
}

void
Figured_bass_engraver::process_music ()
{
  if (rest_event_)
    {
      figure_ = make_item ("BassFigure", rest_event_->self_scm ());
      figure_->set_property ("text", scm_makfrom0str ("-"));
    }
  else if (figures_.size ())
    {
      SCM proc = get_property ("bassFigureFormatFunction");
      if (ly_is_procedure (proc))
	{
	  SCM l = SCM_EOL;
	  SCM *t = &l;
	  for (int i = 0; i < figures_.size (); i++)
	    {
	      *t = scm_cons (figures_[i]->self_scm (), SCM_EOL);
	      t = SCM_CDRLOC (*t);
	    }
	  figure_ = make_item ("BassFigure", figures_[0]->self_scm ());
	  scm_call_3 (proc, l, context ()->self_scm (),
		      figure_->self_scm ());
	}
    }
}

ADD_TRANSLATOR (Figured_bass_engraver,
		/* descr */ "Make figured bass numbers.",
		/* creats*/ "BassFigure",
		/* accepts */ "rest-event bass-figure-event",
		/* acks  */ "",
		/* reads */ "bassFigureFormatFunction",
		/* write */ "");
