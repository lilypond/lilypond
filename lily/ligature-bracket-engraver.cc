/*   
  ligature-bracket-engraver.cc -- implement Ligature_bracket_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2002 Juergen Reuter <reuter@ipd.uka.de>
  
 */
#include "ligature-engraver.hh"
#include "note-column.hh"
#include "tuplet-bracket.hh"
#include "spanner.hh"
#include "warn.hh"

class Ligature_bracket_engraver : public Ligature_engraver
{
protected:
  virtual Spanner *create_ligature_spanner ();
  virtual void acknowledge_grob (Grob_info);

public:
  TRANSLATOR_DECLARATIONS(Ligature_bracket_engraver);

private:
  void typeset_ligature_bracket ();
};


Ligature_bracket_engraver::Ligature_bracket_engraver ()
{
}

Spanner *
Ligature_bracket_engraver::create_ligature_spanner ()
{
  return new Spanner (get_property ("LigatureBracket"));
}

void
Ligature_bracket_engraver::acknowledge_grob (Grob_info info)
{
  if (ligature_)
    {
      if (Note_column::has_interface (info.grob_))
	{
	  Tuplet_bracket::add_column (ligature_, dynamic_cast<Item*> (info.grob_));
	}
      else Ligature_engraver::acknowledge_grob (info);
    }
}

ENTER_DESCRIPTION(Ligature_bracket_engraver,
/* descr */       "Handles Ligature_events by engraving Ligature brackets.",
/* creats*/       "TupletBracket",
/* accepts */     "ligature-event abort-event",
/* acks  */      "ligature-head-interface rest-interface note-column-interface",
/* reads */       "",
/* write */       "");
