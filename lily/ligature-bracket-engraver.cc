/*   
  ligature-bracket-engraver.cc -- implement Ligature_bracket_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2002 Juergen Reuter <reuter@ipd.uka.de>
  
 */
#include "ligature-engraver.hh"
#include "spanner.hh"

class Ligature_bracket_engraver : public Ligature_engraver
{
protected:
  virtual Spanner *create_ligature_spanner ();

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

ENTER_DESCRIPTION(Ligature_bracket_engraver,
/* descr */       "Handles Ligature_events by engraving Ligature brackets.",
/* creats*/       "LigatureBracket",
/* accepts */     "",
/* acks  */      "ligature-head-interface rest-interface",
/* reads */       "",
/* write */       "");
