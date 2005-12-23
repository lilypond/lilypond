/*
  melody-engraver.cc -- implement Melody_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/


#include "engraver.hh"

#include "item.hh"
#include "melody-spanner.hh"
#include "pointer-group-interface.hh"

/**
   Make stems upon receiving noteheads.
*/
class Melody_engraver : public Engraver
{
  Grob *melody_item_;
protected:
  
  DECLARE_ACKNOWLEDGER (stem);
  TRANSLATOR_DECLARATIONS (Melody_engraver);
};


Melody_engraver::Melody_engraver ()
{
  melody_item_ = 0;
}

void
Melody_engraver::acknowledge_stem (Grob_info info)
{
  extract_grob_set (info.grob (), "rests", rests);
  if (rests.size ())
    melody_item_ = 0;
  else
    {
      if (!melody_item_)
	melody_item_ = make_item ("MelodyItem", info.grob ()->self_scm ());

      Melody_spanner::add_stem (melody_item_, info.grob ());
    }
}

#include "translator.icc"
ADD_ACKNOWLEDGER (Melody_engraver, stem);
ADD_TRANSLATOR (Melody_engraver,
		"Create information for context dependent typesetting decisions. ",
		"MelodyItem",
		"",
		"",
		"");
		
