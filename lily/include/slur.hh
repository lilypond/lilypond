/*
  slur.hh -- part of GNU LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef SLUR_HH
#define SLUR_HH

#include "directional-spanner.hh"
#include "lily-proto.hh"
#include "parray.hh"
#include "bow.hh"
#include "curve.hh"

/**
  A #Bow# which tries to drape itself around the stems too.
 */
class Slur : public Bow {
public:
  Link_array<Note_column> encompass_arr_;
  void add (Note_column*);

protected:
  virtual Molecule* brew_molecule_p () const;
  Array<Offset> get_notes () const;
  Array<Offset> get_controls () const;

  virtual void set_default_dir();
  virtual void do_post_processing();
  virtual void do_add_processing ();
  virtual void do_pre_processing ();
  virtual void do_substitute_dependency (Score_elem*, Score_elem*);
  virtual Real height_f () const;

  SCORE_ELEM_CLONE(Slur);
  DECLARE_MY_RUNTIME_TYPEINFO;
};

#endif // SLUR_HH


