/*
  lyric-number-engraver.cc -- implement Stanza_number_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2000--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>, Glen Prideaux <glenprideaux@iname.com>
*/

#include "engraver.hh"
#include "side-position-interface.hh"

class Stanza_number_engraver : public Engraver
{
  Item *text_;

  /*
    This is naughty, since last_stanza_ may be GCd from under us.  But
    since we don't look at the contents, we are/should be (knock on
    wood) OK.
  */
  SCM last_stanza_;
public:
  TRANSLATOR_DECLARATIONS (Stanza_number_engraver);
  void process_music ();
  void stop_translation_timestep ();
  DECLARE_ACKNOWLEDGER(lyric_syllable);
};

/*
  TODO: should make engraver that collects all the stanzas on a higher
  level, and then groups them to the side. Stanza numbers should be
  all aligned.
*/

Stanza_number_engraver::Stanza_number_engraver ()
{
  text_ = 0;
}

void
Stanza_number_engraver::process_music ()
{
  SCM stanza = get_property ("stanza");

  if (scm_is_string (stanza) && stanza != last_stanza_)
    {
      last_stanza_ = stanza;

      text_ = make_item ("StanzaNumber", SCM_EOL);
      text_->set_property ("text", stanza);
    }
}

void
Stanza_number_engraver::acknowledge_lyric_syllable (Grob_info inf)
{
  if (text_)
    {
      Side_position_interface::add_support (text_, inf.grob ());
    }
}

void
Stanza_number_engraver::stop_translation_timestep ()
{
  text_ = 0;
}

#include "translator.icc"

ADD_ACKNOWLEDGER(Stanza_number_engraver,lyric_syllable);
ADD_TRANSLATOR (Stanza_number_engraver,
		/* descr */ "",
		/* creats*/ "StanzaNumber",
		/* accepts */ "",
		/* reads */ "stanza",
		/* write */ "");
