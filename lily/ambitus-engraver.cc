/*
  ambitus-engraver.cc -- implement Ambitus_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2002--2004 Juergen Reuter <reuter@ipd.uka.de>
*/

#include "engraver.hh"
#include "item.hh"
#include "note-head.hh"
#include "staff-symbol-referencer.hh"
#include "event.hh"
#include "pitch.hh"
#include "pitch-interval.hh"
#include "protected-scm.hh"



/*
 * This class implements an engraver for ambitus grobs.
 *
 * TODO: There are quite some conceptional issues left open:
 *
 * - Many publishers put ambitus _before_ the first occurrence of a
 * clef.  Hence, formally the pitches are undefined in this case.  Of
 * course, one could always silently assume that ambitus pitches refer
 * to the first occurrence of a clef.  Or should we, by default, put
 * the ambitus always after the first clef, if any?
 *
 * - Enharmonically equal pitches: Assume piece contains once a "gis",
 * another time an "aes" as highest pitch.  Which one should be
 * selected for the ambitus grob?  The "aes", because it is
 * musically/notationally "higher" than "gis"?  Or "gis", because (if
 * using pure temperament) it has a slightly higher frequency?  Or
 * that pitch that come closer to the key signature?  But there may be
 * key signature changes in the piece...
 *
 * - Multiple voices in single staff: Assume a vocal piece of music,
 * where the soprano voice and the alto voice are put into the same
 * staff (this is generally a bad idea, but unfortunately common
 * practice).  Then, there probably should be two ambitus grobs, one
 * for each voice.  But how can you see which ambitus grob refers to
 * which voice?  Most probably you can guess it from the fact that the
 * ambitus of the alto voice typically lies in a lower range than that
 * of the soprano voice, but this is just a heuristic rather than a
 * generally valid rule.  In the case of only two voices, using stems
 * in the ambitus grob might help, but probably looks quite ugly.
 *
 * - If a piece consists of several loosely coupled sections, should
 * there be multiple ambitus grobs allowed, one for each section?
 * Then there probably should be some "\ambitus" event added to
 * mudela, stating where an ambitus grob should be placed.  This
 * ambitus grob should then represent the ambitus in the range of time
 * between this "\ambitus" event and the next one (or the end of the
 * piece, if there is no more such event).  To be compliant with the
 * current implementation, we might implicitly assume an "\ambitus"
 * event at the beginning of the piece, but then the question where
 * to put this first ambitus grob (before/after the clef?) becomes
 * even more urgent.
 *
 * - Incipits of transcribed music may need special treatment for
 * ambitus, since, for readability, the ambitus most probably should
 * not refer to the ancient clefs of the incipit, but rather to the
 * clefs used in the transcribed parts.
 */
class Ambitus_engraver : public Engraver
{
public:
TRANSLATOR_DECLARATIONS (Ambitus_engraver);
  virtual void process_music ();
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();
  virtual void finalize ();

private:
  void create_ambitus ();
  Item *ambitus_;
  Drul_array<Item *> heads_;
  Drul_array<Item *> accidentals_;
  Pitch_interval pitch_interval_;
  bool is_typeset_;
  int start_c0_;
  Protected_scm start_key_sig_;
};

void
Ambitus_engraver::create_ambitus ()
{
  ambitus_ = make_item ("Ambitus",SCM_EOL);
  is_typeset_ = false;		
}


Ambitus_engraver::Ambitus_engraver ()
{
  ambitus_ = 0;
  is_typeset_ = false;
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
    {
      create_ambitus ();
    }
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

      Direction d = DOWN;
      do
	{
	  heads_[d] = make_item ("AmbitusNoteHead", SCM_EOL);
	  accidentals_[d] = make_item ("AmbitusAccidental", SCM_EOL);
	  heads_[d]->set_property ("accidental-grob", accidentals_[d]->self_scm ());
	}
      while (flip (&d) != DOWN);
      is_typeset_ = true;
    }
}

void
Ambitus_engraver::acknowledge_grob (Grob_info info)
{
  Item *item = dynamic_cast <Item *>(info.grob_);
  if (item)
    {
      if (Note_head::has_interface (info.grob_))
	{
	  Music *nr = info.music_cause ();
	  if (nr && nr->is_mus_type ("note-event"))
	    {
	      Pitch pitch = *unsmob_pitch (nr->get_property ("pitch"));
	      pitch_interval_.add_point (pitch);
	    }
	}
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
	  heads_[d]->set_property ("position",
				   scm_from_int (start_c0_ +
						 p.steps ()));

	  SCM handle = scm_assoc (scm_cons (scm_from_int (p.get_octave ()),
					    scm_from_int (p.get_notename ())),
				  start_key_sig_);

	  int sig_alter = (handle != SCM_BOOL_F) ? ly_scm2int (ly_car (handle)) : 0;
	  if (sig_alter == p.get_alteration ())
	    {
	      accidentals_[d]->suicide();
	      heads_[d]->set_property ("accidental-grob", SCM_EOL);
	    }
	  else
	    {
	      accidentals_[d]->set_property ("accidentals",
					     scm_list_1 (scm_from_int (p.get_alteration ())));
	    }
	}
      while (flip (&d) != DOWN);

      ambitus_->set_property ("note-heads", scm_list_2 (heads_[DOWN]->self_scm (),
							heads_[UP]->self_scm ()));
    }
  else
    {
      Direction d = DOWN;
      do
	{
	  accidentals_[d]->suicide();
	  heads_[d]->suicide();
   	}
      while (flip (&d) != DOWN);
      ambitus_->suicide();
    }
}

ENTER_DESCRIPTION (Ambitus_engraver,
/* descr */       "",
/* creats*/       "Ambitus",
/* accepts */ "",
/* acks  */     "note-head-interface",
/* reads */       "",
/* write */       "");
