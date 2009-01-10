/*
  stanza-number-align-engraver.cc -- implement Stanza_number_align_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2004--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "context.hh"
#include "engraver.hh"
#include "note-head.hh"
#include "lyric-extender.hh"
#include "pointer-group-interface.hh"
#include "side-position-interface.hh"

#include "translator.icc"

class Stanza_number_align_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Stanza_number_align_engraver);

protected:
  vector<Grob*> lyrics_;
  vector<Grob*> stanza_numbers_;

  DECLARE_ACKNOWLEDGER (lyric_syllable);
  DECLARE_ACKNOWLEDGER (stanza_number);
  void stop_translation_timestep ();
};

Stanza_number_align_engraver::Stanza_number_align_engraver ()
{
}

void
Stanza_number_align_engraver::acknowledge_lyric_syllable (Grob_info gi)
{
  Grob *h = gi.grob ();
  lyrics_.push_back (h);
}

void
Stanza_number_align_engraver::acknowledge_stanza_number (Grob_info gi)
{
  Grob *h = gi.grob ();
  stanza_numbers_.push_back (h);
}

void
Stanza_number_align_engraver::stop_translation_timestep ()
{
  for (vsize i = lyrics_.size (); i--;)
    for (vsize j = stanza_numbers_.size (); j--;)
      Side_position_interface::add_support (stanza_numbers_[j], lyrics_[i]);

  stanza_numbers_.clear ();
  lyrics_.clear ();
}

ADD_ACKNOWLEDGER (Stanza_number_align_engraver, lyric_syllable);
ADD_ACKNOWLEDGER (Stanza_number_align_engraver, stanza_number);

ADD_TRANSLATOR (Stanza_number_align_engraver,
		/* doc */
		"This engraver ensures that stanza numbers are neatly"
		" aligned.",

		/* create */
		"",

		/* read */
		"",

		/* write */
		"");

