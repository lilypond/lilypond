
/*
  stanza-number-align-engraver.cc -- implement

  source file of the GNU LilyPond music typesetter

  (c) 2004--2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "context.hh"
#include "engraver.hh"
#include "note-head.hh"
#include "lyric-extender.hh"
#include "group-interface.hh"
#include "side-position-interface.hh"

class Stanza_number_align_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Stanza_number_align_engraver);

protected:
  Link_array<Grob> lyrics_;
  Link_array<Grob> stanza_numbers_;
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();
};

Stanza_number_align_engraver::Stanza_number_align_engraver ()
{

}

void
Stanza_number_align_engraver::acknowledge_grob (Grob_info gi)
{
  Grob *h = gi.grob_;

  if (h->internal_has_interface (ly_symbol2scm ("lyric-syllable-interface")))
    lyrics_.push (h);
  else if (h->internal_has_interface (ly_symbol2scm ("stanza-number-interface")))
    stanza_numbers_.push (h);
}

void
Stanza_number_align_engraver::stop_translation_timestep ()
{
  for (int i = lyrics_.size (); i--;)
    for (int j = stanza_numbers_.size (); j--;)
      Side_position_interface::add_support (stanza_numbers_[j], lyrics_[i]);

  stanza_numbers_.clear ();
  lyrics_.clear ();
}

ADD_TRANSLATOR (Stanza_number_align_engraver,
		"This engraver ensures that stanza numbers are neatly aligned. ",
		"",
		"",
		"stanza-number-interface lyric-syllable-interface ",
		"",
		"");

