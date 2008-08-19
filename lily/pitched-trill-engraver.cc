/*
  pitched-trill-engraver.cc -- implement Pitched_trill_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2005--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "engraver.hh"

#include "axis-group-interface.hh"
#include "context.hh"
#include "dots.hh"
#include "item.hh"
#include "note-head.hh"
#include "pitch.hh"
#include "pointer-group-interface.hh"
#include "side-position-interface.hh"
#include "stream-event.hh"
#include "warn.hh"

class Pitched_trill_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Pitched_trill_engraver);

protected:
  DECLARE_ACKNOWLEDGER (note_head);
  DECLARE_ACKNOWLEDGER (dots);
  DECLARE_ACKNOWLEDGER (trill_spanner);
  void stop_translation_timestep ();

private:
  Item *trill_head_;
  Item *trill_group_;
  Item *trill_accidental_;

  vector<Grob*> heads_;

  void make_trill (Stream_event *);
};

Pitched_trill_engraver::Pitched_trill_engraver ()
{
  trill_head_ = 0;
  trill_group_ = 0;
  trill_accidental_ = 0;
}

void
Pitched_trill_engraver::acknowledge_dots (Grob_info info)
{
  heads_.push_back (info.grob ());
}
void
Pitched_trill_engraver::acknowledge_note_head (Grob_info info)
{
  heads_.push_back (info.grob ());
}

void
Pitched_trill_engraver::acknowledge_trill_spanner (Grob_info info)
{
  Stream_event *ev = info.event_cause ();
  if (ev
      && ev->in_event_class ("trill-span-event")
      && to_dir (ev->get_property ("span-direction")) == START
      && unsmob_pitch (ev->get_property ("pitch")))
    make_trill (ev);
}

void
Pitched_trill_engraver::make_trill (Stream_event *ev)
{
  SCM scm_pitch = ev->get_property ("pitch");
  Pitch *p = unsmob_pitch (scm_pitch);

  SCM keysig = get_property ("localKeySignature");

  SCM key = scm_cons (scm_from_int (p->get_octave ()),
		      scm_from_int (p->get_notename ()));

  int bn = measure_number (context());

  SCM handle = scm_assoc (key, keysig);
  if (handle != SCM_BOOL_F)
    {
      bool same_bar = (bn == robust_scm2int (scm_caddr (handle), 0));
      bool same_alt
	= (p->get_alteration () == robust_scm2rational (scm_cadr (handle), 0));

      if (!same_bar || (same_bar && !same_alt))
	handle = SCM_BOOL_F;
    }

  bool print_acc
    = (handle == SCM_BOOL_F) || p->get_alteration () == Rational (0)
    || (ev->get_property ("force-accidental") == SCM_BOOL_T);

  if (trill_head_)
    {
      programming_error ("already have a trill head.");
      trill_head_ = 0;
    }

  trill_head_ = make_item ("TrillPitchHead", ev->self_scm ());
  SCM c0scm = get_property ("middleCPosition");

  int c0 = scm_is_number (c0scm) ? scm_to_int (c0scm) : 0;

  trill_head_->set_property ("staff-position",
			     scm_from_int (unsmob_pitch (scm_pitch)->steps ()
					   + c0));

  trill_group_ = make_item ("TrillPitchGroup", ev->self_scm ());
  trill_group_->set_parent (trill_head_, Y_AXIS);

  Axis_group_interface::add_element (trill_group_, trill_head_);

  if (print_acc)
    {
      trill_accidental_ = make_item ("TrillPitchAccidental", ev->self_scm ());

      // fixme: naming -> alterations
      trill_accidental_->set_property ("alteration", ly_rational2scm (p->get_alteration ()));
      Side_position_interface::add_support (trill_accidental_, trill_head_);
      
      trill_head_->set_object ("accidental-grob", trill_accidental_->self_scm ());
      trill_accidental_->set_parent (trill_head_, Y_AXIS);
      Axis_group_interface::add_element (trill_group_, trill_accidental_);
    }
}

void
Pitched_trill_engraver::stop_translation_timestep ()
{
  if (trill_group_)
    for (vsize i = 0; i < heads_.size (); i++)
      Side_position_interface::add_support (trill_group_, heads_[i]);

  heads_.clear ();
  trill_head_ = 0;
  trill_group_ = 0;
  trill_accidental_ = 0;
}


#include "translator.icc"

ADD_ACKNOWLEDGER (Pitched_trill_engraver, note_head);
ADD_ACKNOWLEDGER (Pitched_trill_engraver, dots);
ADD_ACKNOWLEDGER (Pitched_trill_engraver, trill_spanner);

ADD_TRANSLATOR (Pitched_trill_engraver,
		/* doc */
		"Print the bracketed note head after a note head with trill.",

		/* create */
		"TrillPitchHead "
		"TrillPitchAccidental "
		"TrillPitchGroup ",

		/* read */
		"",

		/* write */
		""
		);
