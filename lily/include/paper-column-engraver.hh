/*
  paper-column-engraver.hh -- declare Paper_column_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef PAPER_COLUMN_ENGRAVER_HH
#define PAPER_COLUMN_ENGRAVER_HH

#include "engraver.hh"

class Paper_column_engraver : public Engraver
{
  void make_columns ();
  void set_columns (Paper_column *, Paper_column *);
  TRANSLATOR_DECLARATIONS (Paper_column_engraver);

protected:
  void stop_translation_timestep ();
  void start_translation_timestep ();
  void process_music ();
  virtual void initialize ();
  virtual void finalize ();
  virtual bool try_music (Music *);

  DECLARE_ACKNOWLEDGER (item);
  DECLARE_ACKNOWLEDGER (note_spacing);
  DECLARE_ACKNOWLEDGER (staff_spacing);

  System *system_;
  Music *break_event_;
  int breaks_;			// used for stat printing
  Paper_column *command_column_;
  Paper_column *musical_column_;
  Link_array<Item> items_;
  bool first_;
  Moment last_moment_;
public:
  // ug.h 
  void forbid_breaks ();
};

#endif /* PAPER_COLUMN_ENGRAVER_HH */
