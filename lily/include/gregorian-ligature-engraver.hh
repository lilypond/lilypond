/*   
  gregorian-ligature-engraver.hh -- declare Gregorian_ligature_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2003 Juergen Reuter <reuter@ipd.uka.de>
  
 */
#ifndef GREGORIAN_LIGATURE_ENGRAVER_HH
#define GREGORIAN_LIGATURE_ENGRAVER_HH

#include "ligature-engraver.hh"

class Gregorian_ligature_engraver : public Ligature_engraver
{
  Music *pes_or_flexa_req_;

public:
  TRANSLATOR_DECLARATIONS(Gregorian_ligature_engraver);

protected:
  virtual bool try_music (Music *);
  virtual void typeset_ligature (Spanner *ligature,
				 Array<Grob_info> primitives);
  virtual void transform_heads (Spanner *ligature,
				Array<Grob_info> primitives); /* abstract method */
  virtual void start_translation_timestep ();
  void get_set_column (Item *, Paper_column *);
};

#endif // GREGORIAN_LIGATURE_ENGRAVER_HH
