/*
  textspanner.hh -- part of GNU LilyPond

  (c) 1996--1999 Han-Wen Nienhuys
*/

#ifndef TEXTSPANNER_HH
#define TEXTSPANNER_HH

#include "string.hh"
#include "directional-spanner.hh"
#include "text-def.hh"

/** a spanner which puts texts on top of other spanners.  Use for
  triplets, volta, ottava, etc.  */
class Text_spanner : public Spanner {
public:
  Directional_spanner * support_span_l_;
  General_script_def * spec_p_;
  Offset text_off_;
  

  void set_support (Directional_spanner*);
  Text_spanner();
  Text_spanner (Text_spanner const&);
protected:
  VIRTUAL_COPY_CONS(Score_element);
  ~Text_spanner();
  virtual void do_add_processing ();
  virtual void do_substitute_element_pointer (Score_element*,Score_element*);
  virtual void do_pre_processing();
  virtual void do_post_processing();
  virtual Interval height() const ;
  virtual Molecule* do_brew_molecule_p() const;
  virtual void do_print() const;
};
#endif // TEXTSPANNER_HH

