/*
  ligature-engraver.hh -- declare Ligature_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2002--2008 Juergen Reuter <reuter@ipd.uka.de>
*/

#ifndef LIGATURE_ENGRAVER_HH
#define LIGATURE_ENGRAVER_HH

#include "engraver.hh"
#include "moment.hh"

class Ligature_engraver : public Engraver
{
protected:
  Ligature_engraver ();
  void stop_translation_timestep ();
  virtual void finalize ();

  DECLARE_ACKNOWLEDGER (rest);
  DECLARE_ACKNOWLEDGER (note_head);
  virtual void listen_ligature (Stream_event *ev);
  void process_music ();
  virtual Spanner *create_ligature_spanner () = 0;
  virtual void typeset_ligature (Spanner *ligature,
				 vector<Grob_info> primitives) = 0;
  virtual Spanner *current_ligature ();
  SCM brew_ligature_primitive_proc;

public:
  // no TRANSLATOR_DECLARATIONS (Ligature_engraver) needed since this
  // class is abstract

private:
  Drul_array<Stream_event *> events_drul_;

  Spanner *ligature_;
  vector<Grob_info> primitives_;

  Spanner *finished_ligature_;
  vector<Grob_info> finished_primitives_;

  Stream_event *prev_start_event_;

  // moment where ligature started.
  Moment ligature_start_mom_;

  Grob *last_bound_;
};

#endif // LIGATURE_ENGRAVER_HH
