/*
  ambitus-engraver.cc -- implement Ambitus_engraver

  source file of the GNU LilyPond music typesetter

  (C) 2002 Juergen Reuter <reuter@ipd.uka.de>
*/

#include "engraver.hh"
#include "item.hh"
#include "note-head.hh"
#include "staff-symbol-referencer.hh"
#include "musical-request.hh"
#include "pitch.hh"

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
 * Then there probably should be some "\ambitus" request added to
 * mudela, stating where an ambitus grob should be placed.  This
 * ambitus grob should then represent the ambitus in the range of time
 * between this "\ambitus" request and the next one (or the end of the
 * piece, if there is no more such request).  To be compliant with the
 * current implementation, we might implicitly assume an "\ambitus"
 * request at the beginning of the piece, but then the question where
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
TRANSLATOR_DECLARATIONS(Ambitus_engraver);
  virtual void process_music ();
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();
  virtual void finalize ();

private:
  void create_ambitus ();
  Item *ambitus_;
  int/*bool*/ is_typeset;
  Pitch pitch_min, pitch_max;
};

Ambitus_engraver::Ambitus_engraver ()
{
  ambitus_ = 0;
  is_typeset = 0;

  /*
   * (pitch_min > pitch_max) means that pitches are not yet
   * initialized
   */
  pitch_min = Pitch (0, 0, +1);
  pitch_max = Pitch (0, 0, -1);
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
  if (!ambitus_) {
    create_ambitus ();
  }
}

void
Ambitus_engraver::stop_translation_timestep ()
{
  if (ambitus_ && !is_typeset)
    {
      /*
       * Evaluate centralCPosition not until now, since otherwise we
       * may then oversee a clef that is defined in a staff context if
       * we are in a voice context; centralCPosition would then be
       * assumed to be 0.
       */
      SCM c0 = get_property ("centralCPosition");
      ambitus_->set_grob_property ("c0-position", c0);

      /*
       * Similar for keySignature.
       */
      SCM key_signature = get_property ("keySignature");
      ambitus_->set_grob_property ("accidentals", key_signature);

      typeset_grob (ambitus_);
      is_typeset = 1;
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
	  Note_req *nr = dynamic_cast<Note_req*> (info.music_cause ());
	  if (nr)
	    {
	      Pitch pitch = *unsmob_pitch (nr->get_mus_property ("pitch"));
	      if (Pitch::compare (pitch_min, pitch_max) > 0) // already init'd?
		{
		  // not yet init'd; use current pitch to init min/max
		  pitch_min = pitch;
		  pitch_max = pitch;
		}
	      else if (Pitch::compare (pitch, pitch_max) > 0) // new max?
		{
		  pitch_max = pitch;
		}
	      else if (Pitch::compare (pitch, pitch_min) < 0) // new min?
		{
		  pitch_min = pitch;
		}
	    }
	}
    }
}

void
Ambitus_engraver::create_ambitus ()
{
  SCM basicProperties = get_property ("Ambitus");
  ambitus_ = new Item (basicProperties); is_typeset = 0;
  announce_grob (ambitus_, SCM_EOL);
}

void
Ambitus_engraver::finalize ()
{
  if (ambitus_)
    {
      if (Pitch::compare (pitch_min, pitch_max) <= 0)
	{
	  ambitus_->set_grob_property ("pitch-min",
					 pitch_min.smobbed_copy ());
	  ambitus_->set_grob_property ("pitch-max",
					 pitch_max.smobbed_copy ());
	}
      else // have not seen any pitch, so forget about the ambitus
	{
	  /*
	   * Do not print a warning on empty ambitus range, since this
	   * most probably arises from an empty voice, such as shared
	   * global timesig/clef definitions.
	   */
#if 0
	  ambitus_->warning("empty ambitus range [ignored]");
#endif
	  ambitus_->suicide();
	}
    }
}

ENTER_DESCRIPTION(Ambitus_engraver,
/* descr */       "",
/* creats*/       "Ambitus",
/* accepts */ "general-music",
/* acks  */     "note-head-interface",
/* reads */       "",
/* write */       "");
