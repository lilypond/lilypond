/*   
  ligature-engraver.hh -- declare Ligature_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2002--2004 Juergen Reuter <reuter@ipd.uka.de>
  
 */
#ifndef LIGATURE_ENGRAVER_HH
#define LIGATURE_ENGRAVER_HH

#include "engraver.hh"

class Ligature_engraver : public Engraver
{
protected:
  virtual void stop_translation_timestep ();
  virtual void finalize ();

  virtual void acknowledge_grob (Grob_info);
  virtual bool try_music (Music*);
  virtual void process_music ();
  virtual Spanner *create_ligature_spanner (); /* abstract method */
  virtual void typeset_ligature (Spanner *ligature,
				 Array<Grob_info> primitives); /* abstract method */
  virtual Spanner *current_ligature ();
  SCM brew_ligature_primitive_proc;

public:
  TRANSLATOR_DECLARATIONS(Ligature_engraver);

private:
  Drul_array<Music*> reqs_drul_;
  
  Spanner *ligature_;
  Array<Grob_info> primitives_;

  Spanner *finished_ligature_;
  Array<Grob_info> finished_primitives_;

  Music *prev_start_req_;

  // moment where ligature started.
  Moment ligature_start_mom_;

  Grob *last_bound_;

  void override_stencil_callback ();
  void revert_stencil_callback ();
};

#endif // LIGATURE_ENGRAVER_HH
