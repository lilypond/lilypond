/*
  gregorian-ligature-engraver.hh -- declare Gregorian_ligature_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2003--2009 Juergen Reuter <reuter@ipd.uka.de>
*/
#ifndef GREGORIAN_LIGATURE_ENGRAVER_HH
#define GREGORIAN_LIGATURE_ENGRAVER_HH

#include "coherent-ligature-engraver.hh"

class Gregorian_ligature_engraver : public Coherent_ligature_engraver
{
  Stream_event *pes_or_flexa_req_;

public:
  // no TRANSLATOR_DECLARATIONS (Gregorian_ligature_engraver) needed
  // since this class is abstract

protected:
  Gregorian_ligature_engraver ();

  virtual void listen_pes_or_flexa (Stream_event *ev);
  virtual void build_ligature (Spanner *ligature, vector<Grob_info> primitives);
  virtual void transform_heads (Spanner *ligature,
				vector<Grob_info> primitives) = 0;
  void stop_translation_timestep ();
};

#endif // GREGORIAN_LIGATURE_ENGRAVER_HH
