/*
  textspanner.hh -- part of GNU LilyPond

  (c) 1996--1998 Han-Wen Nienhuys
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
  DECLARE_MY_RUNTIME_TYPEINFO;

  void set_support (Directional_spanner*);
  Text_spanner();
  Text_spanner (Text_spanner const&);
protected:
  SCORE_ELEMENT_CLONE(Text_spanner);
  ~Text_spanner();
  virtual void do_add_processing ();
  virtual void do_substitute_dependency (Score_element*,Score_element*);
  virtual void do_pre_processing();
  virtual void do_post_processing();
  virtual Interval height() const ;
  virtual Molecule* brew_molecule_p() const;
  virtual void do_print() const;
};
#endif // TEXTSPANNER_HH

