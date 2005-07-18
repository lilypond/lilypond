/*
  gregorian-ligature-engraver.hh -- declare Gregorian_ligature_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2003--2005 Juergen Reuter <reuter@ipd.uka.de>
*/
#ifndef GREGORIAN_LIGATURE_ENGRAVER_HH
#define GREGORIAN_LIGATURE_ENGRAVER_HH

#include "coherent-ligature-engraver.hh"

class Gregorian_ligature_engraver : public Coherent_ligature_engraver
{
  Music *pes_or_flexa_req_;

public:
  TRANSLATOR_DECLARATIONS (Gregorian_ligature_engraver);

protected:
  virtual bool try_music (Music *);
  virtual void build_ligature (Spanner *ligature, Array<Grob_info> primitives);
  virtual void transform_heads (Spanner *ligature,
				Array<Grob_info> primitives); /* abstract method */
  PRECOMPUTED_VIRTUAL void stop_translation_timestep ();
};

#endif // GREGORIAN_LIGATURE_ENGRAVER_HH
