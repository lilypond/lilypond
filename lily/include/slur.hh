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

/**
  A #Bow# which tries to drape itself around the stems too.
 */
class Slur : public Bow
{
public:
  Link_array<Note_column> encompass_arr_;
  void add (Note_column*);

  Slur ();

  SCORE_ELEM_CLONE(Slur);
  DECLARE_MY_RUNTIME_TYPEINFO;

protected:
  virtual Array<Offset> get_encompass_offset_arr () const;

  virtual void set_default_dir ();
  virtual void do_post_processing ();
  virtual void do_add_processing ();
  virtual void do_pre_processing ();
  virtual void do_substitute_dependency (Score_elem*, Score_elem*);
};

#endif // SLUR_HH


