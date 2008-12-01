/*
  font-size-engraver.cc -- implement Font_size_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2001--2008  Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "grob.hh"
#include "engraver.hh"

class Font_size_engraver : public Engraver
{

  TRANSLATOR_DECLARATIONS (Font_size_engraver);
protected:
  DECLARE_ACKNOWLEDGER (font);
  void process_music ();
  Real size;
private:
};

Font_size_engraver::Font_size_engraver ()
{
  size = 0.0;
}

void
Font_size_engraver::process_music ()
{
  size = robust_scm2double (get_property ("fontSize"), 0.0);
}

void
Font_size_engraver::acknowledge_font (Grob_info gi)
{
  /*
    We only want to process a grob once.
  */
  if (!size)
    return;

  if (gi.context () != context ())
    return;

  Real font_size = size
    + robust_scm2double (gi.grob ()->get_property ("font-size"), 0);
  gi.grob ()->set_property ("font-size", scm_from_double (font_size));
}

#include "translator.icc"

ADD_ACKNOWLEDGER (Font_size_engraver, font);
ADD_TRANSLATOR (Font_size_engraver,
		/* doc */
		"Put @code{fontSize} into @code{font-size} grob property.",

		/* create */
		"",

		/* read */
		"fontSize ",

		/* write */
		""
		);
