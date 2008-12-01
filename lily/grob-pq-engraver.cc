/*
  grob-pq-engraver.cc -- implement Grob_pq_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2001--2008  Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "context.hh"
#include "engraver.hh"
#include "grob.hh"
#include "warn.hh"

struct Grob_pq_entry
{
  Grob *grob_;
  Moment end_;
};

bool
operator< (Grob_pq_entry const &a, Grob_pq_entry const &b)
{
  return a.end_ < b.end_;
}

class Grob_pq_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Grob_pq_engraver);
protected:
  virtual void initialize ();
  DECLARE_ACKNOWLEDGER (grob);
  void start_translation_timestep ();
  void stop_translation_timestep ();
  void process_acknowledged ();
  
  vector<Grob_pq_entry> started_now_;
};

Grob_pq_engraver::Grob_pq_engraver ()
{
}

void
Grob_pq_engraver::initialize ()
{
  context ()->set_property ("busyGrobs", SCM_EOL);
}

LY_DEFINE (ly_grob_pq_less_p, "ly:grob-pq<?",
	   2, 0, 0, (SCM a, SCM b),
	   "Compare two grob priority queue entries."
	   "  This is an internal function.")
{
  if (Moment::compare (*unsmob_moment (scm_car (a)),
		       *unsmob_moment (scm_car (b))) < 0)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

void
Grob_pq_engraver::acknowledge_grob (Grob_info gi)
{
  Stream_event *ev = gi.event_cause ();

  if (ev
      && !gi.grob ()->internal_has_interface (ly_symbol2scm ("multi-measure-interface")))
    {
      Moment n = now_mom ();
      Moment l = get_event_length (ev, n);

      if (!l.to_bool ())
	return;

      Moment end = n + l;

      Grob_pq_entry e;
      e.grob_ = gi.grob ();
      e.end_ = end;

      started_now_.push_back (e);
    }
}

void
Grob_pq_engraver::process_acknowledged ()
{
  vector_sort (started_now_, less<Grob_pq_entry> ());
  SCM lst = SCM_EOL;
  SCM *tail = &lst;
  for (vsize i = 0; i < started_now_.size (); i++)
    {
      *tail = scm_acons (started_now_[i].end_.smobbed_copy (),
			 started_now_[i].grob_->self_scm (),
			 SCM_EOL);
      tail = SCM_CDRLOC (*tail);
    }

  SCM busy = get_property ("busyGrobs");
  busy = scm_merge_x (lst, busy, ly_grob_pq_less_p_proc);
  context ()->set_property ("busyGrobs", busy);

  started_now_.clear ();
}

void
Grob_pq_engraver::stop_translation_timestep ()
{
  Moment now = now_mom ();
  SCM start_busy = get_property ("busyGrobs");
  SCM busy = start_busy;
  while (scm_is_pair (busy) && *unsmob_moment (scm_caar (busy)) == now)
    busy = scm_cdr (busy);

}

void
Grob_pq_engraver::start_translation_timestep ()
{
  Moment now = now_mom ();

  SCM start_busy = get_property ("busyGrobs");
  SCM busy = start_busy;
  while (scm_is_pair (busy) && *unsmob_moment (scm_caar (busy)) < now)
    {
      /*
	The grob-pq-engraver is not water tight, and stuff like
	tupletSpannerDuration confuses it.
      */
      busy = scm_cdr (busy);
    }

  if (start_busy != busy)
    context ()->set_property ("busyGrobs", busy);
}

#include "translator.icc"
ADD_ACKNOWLEDGER (Grob_pq_engraver, grob);
ADD_TRANSLATOR (Grob_pq_engraver,
		/* doc */
		"Administrate when certain grobs (e.g., note heads) stop"
		" playing.",

		/* create */
		"",

		/* read */
		"busyGrobs ",

		/* write */
		"busyGrobs "
		);
