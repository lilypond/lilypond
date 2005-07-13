/*
  ligature-engraver.hh -- declare Ligature_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2002--2005 Juergen Reuter <reuter@ipd.uka.de>
*/
#ifndef LIGATURE_ENGRAVER_HH
#define LIGATURE_ENGRAVER_HH

#include "engraver.hh"
#include "moment.hh"


/*
 * FIXME: Spanner *create_ligature_spanner () and virtual void
 * typeset_ligature (...) are abstract methods, such that we would
 * like to declare them abstract:
 *
 *    virtual Spanner *create_ligature_spanner () = 0;
 *    virtual void typeset_ligature (...) = 0;
 *
 * Unfortunately, clone_const_helper() (as expanded from the
 * TRANSLATOR_DECLARATIONS macro) requires this class to be
 * instantiatable, such that it may not have any abstract virtual
 * functions.  As a workaround, the actually abstract methods are
 * implemented, but produce a programming_error whenever called. --jr
 */

class Ligature_engraver : public Engraver
{
protected:
  virtual void stop_translation_timestep ();
  virtual void finalize ();

  virtual void acknowledge_grob (Grob_info);
  virtual bool try_music (Music *);
  virtual void process_music ();
  virtual Spanner *create_ligature_spanner (); /* abstract method */
  virtual void typeset_ligature (Spanner *ligature,
				 Array<Grob_info> primitives); /* abstract method */
  virtual Spanner *current_ligature ();
  SCM brew_ligature_primitive_proc;

public:
  TRANSLATOR_DECLARATIONS (Ligature_engraver);

private:
  Drul_array<Music *> reqs_drul_;

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
