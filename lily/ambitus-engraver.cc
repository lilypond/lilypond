/*
  ambitus-engraver.cc -- implement Ambitus_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2002--2007 Juergen Reuter <reuter@ipd.uka.de>

  Han-Wen Nienhuys <hanwen@xs4all.nl
*/

#include "engraver.hh"

#include "axis-group-interface.hh"
#include "note-head.hh"
#include "pitch-interval.hh"
#include "pointer-group-interface.hh"
#include "protected-scm.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh" 
#include "stream-event.hh"

#include "translator.icc"

class Ambitus_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Ambitus_engraver);
  void process_music ();
  void acknowledge_note_head (Grob_info);
  void stop_translation_timestep ();
  virtual void finalize ();
  virtual void derived_mark () const;

private:
  void create_ambitus ();
  Item *ambitus_;
  Item *group_;
  Drul_array<Item *> heads_;
  Drul_array<Item *> accidentals_;
  Pitch_interval pitch_interval_;
  bool is_typeset_;
  int start_c0_;
  SCM start_key_sig_;
};

void
Ambitus_engraver::derived_mark () const
{
  scm_gc_mark (start_key_sig_);
}

void
Ambitus_engraver::create_ambitus ()
{
  ambitus_ = make_item ("AmbitusLine", SCM_EOL);
  group_ = make_item ("Ambitus", SCM_EOL);
  Direction d = DOWN;
  do
    {
      heads_[d] = make_item ("AmbitusNoteHead", SCM_EOL);
      accidentals_[d] = make_item ("AmbitusAccidental", SCM_EOL);
      accidentals_[d]->set_parent (heads_[d], Y_AXIS);
      heads_[d]->set_object ("accidental-grob",
			     accidentals_[d]->self_scm ());
      Axis_group_interface::add_element (group_, heads_[d]);
      Axis_group_interface::add_element (group_, accidentals_[d]);
      Side_position_interface::add_support (accidentals_[d], heads_[d]);
    }
  while (flip (&d) != DOWN);

  ambitus_->set_parent (heads_[DOWN], X_AXIS);
  Axis_group_interface::add_element (group_, ambitus_);

  is_typeset_ = false;
}

Ambitus_engraver::Ambitus_engraver ()
{
  ambitus_ = 0;
  heads_[LEFT] = heads_[RIGHT] = 0;
  accidentals_[LEFT] = accidentals_[RIGHT] = 0;
  group_ = 0;
  is_typeset_ = false;
  start_key_sig_ = SCM_EOL;
}

void
Ambitus_engraver::process_music ()
{
  /*
   * Ensure that ambitus is created in the very first timestep (on
   * which lily does not call start_translation_timestep ()).
   * Otherwise, if a voice begins with a rest, the ambitus grob will
   * be placed after the rest.
   */
  if (!ambitus_)
    create_ambitus ();
}

void
Ambitus_engraver::stop_translation_timestep ()
{
  if (ambitus_ && !is_typeset_)
    {
      /*
       * Evaluate middleCPosition not until now, since otherwise we
       * may then oversee a clef that is defined in a staff context if
       * we are in a voice context; middleCPosition would then be
       * assumed to be 0.
       */
      start_c0_ = robust_scm2int (get_property ("middleCPosition"), 0);
      start_key_sig_ = get_property ("keySignature");

      is_typeset_ = true;
    }
}

void
Ambitus_engraver::acknowledge_note_head (Grob_info info)
{
  Stream_event *nr = info.event_cause ();
  if (nr && nr->in_event_class ("note-event"))
    {
      Pitch pitch = *unsmob_pitch (nr->get_property ("pitch"));
      pitch_interval_.add_point (pitch);
    }
}

void
Ambitus_engraver::finalize ()
{
  if (ambitus_ && !pitch_interval_.is_empty ())
    {
      Direction d = DOWN;
      do
	{
	  Pitch p = pitch_interval_[d];
	  heads_[d]->set_property ("staff-position",
				   scm_from_int (start_c0_
						 + p.steps ()));

	  SCM handle = scm_assoc (scm_cons (scm_from_int (p.get_octave ()),
					    scm_from_int (p.get_notename ())),
				  start_key_sig_);

	  if (handle == SCM_BOOL_F)
	    handle = scm_assoc (scm_from_int (p.get_notename ()),
				start_key_sig_);

	  int sig_alter = (handle != SCM_BOOL_F)
	    ? scm_to_int (scm_cdr (handle)) : 0;

	  if (sig_alter == p.get_alteration ())
	    {
	      accidentals_[d]->suicide ();
	      heads_[d]->set_object ("accidental-grob", SCM_EOL);
	    }
	  else
	    {
	      SCM l = scm_list_1 (scm_from_int (p.get_alteration ()));
	      accidentals_[d]->set_property ("accidentals", l);
	    }
	}
      while (flip (&d) != DOWN);


      Pointer_group_interface::add_grob (ambitus_, ly_symbol2scm ("note-heads"), heads_[DOWN]);
      Pointer_group_interface::add_grob (ambitus_, ly_symbol2scm ("note-heads"), heads_[UP]);
    }
  else
    {
      Direction d = DOWN;
      do
	{
	  accidentals_[d]->suicide ();
	  heads_[d]->suicide ();
 	}
      while (flip (&d) != DOWN);

      ambitus_->suicide ();
    }
}

ADD_ACKNOWLEDGER (Ambitus_engraver, note_head);
ADD_TRANSLATOR (Ambitus_engraver,
		/* doc */ "",
		/* create */
		"Ambitus "
		"AmbitusLine "
		"AmbitusNoteHead "
		"AmbitusAccidental",
		/* read */ "",
		/* write */ "");
