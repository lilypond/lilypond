/*   
  ligature-engraver.hh -- declare Ligature_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2002 Juergen Reuter <reuter@ipd.uka.de>
  
 */
#ifndef LIGATUREENGRAVER_HH
#define LIGATUREEENGRAVER_HH

#include "engraver.hh"

class Ligature_engraver : public Engraver
{
protected:
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void finalize ();

  virtual void acknowledge_grob (Grob_info);
  virtual bool try_music (Music*);
  virtual void process_music ();
  virtual void try_stop_ligature ();
  virtual Spanner *create_ligature_spanner ();

  Spanner *finished_ligature_p_;
  Spanner *ligature_p_;
  SCM brew_ligature_primitive_proc;

public:
  TRANSLATOR_DECLARATIONS(Ligature_engraver);

private:
  Drul_array<Span_req*> reqs_drul_;
  
  Span_req *prev_start_req_;

  // moment where ligature started.
  Moment ligature_start_mom_;

  Grob *last_bound;

};

#endif // ENGRAVERGROUP_HH
