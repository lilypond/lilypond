/*
  slur.hh -- declare Slur

  (c) 1996--1999 Han-Wen Nienhuys
*/

#ifndef SLUR_HH
#define SLUR_HH

#include "bow.hh"
#include "rod.hh"

/**
  A #Bow# which tries to drape itself around the stems too.
 */
class Slur : public Bow
{
  bool broken_edge_b ( Direction dir) const;
  bool normal_edge_b ( Direction dir) const;
  Drul_array<Note_column*> extrema () const; 

public:
  Slur ();
  VIRTUAL_COPY_CONS(Score_element);

  void add_column (Note_column*);
  
  Link_array<Note_column> encompass_arr_;

protected:
  virtual Array<Offset> get_encompass_offset_arr () const;

  virtual Direction get_default_dir () const;
  virtual void do_post_processing ();
  virtual void do_add_processing ();
  virtual void do_pre_processing ();
  virtual void do_substitute_element_pointer (Score_element*, Score_element*);
  Array<Rod> get_rods () const;
};

#endif // SLUR_HH


