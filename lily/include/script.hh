/*
  script.hh -- part of GNU LilyPond

  (c) 1996--1999 Han-Wen Nienhuys
*/

#ifndef SCRIPT_HH
#define SCRIPT_HH

#include "staff-side.hh"
#include "item.hh"
  
/**
  Accents that are put over a note-group.
 */
class Script : public Item, public Staff_side {
  Stem *stem_l_;

protected:
  Molecule *do_brew_molecule_p() const;
  virtual void do_substitute_element_pointer (Score_element*,Score_element*);
  virtual void do_print() const;
  virtual Interval symbol_height() const;
  virtual void do_pre_processing();
  virtual Interval do_width() const;
  VIRTUAL_COPY_CONS(Score_element);
private:

  void set_default_dir();
public:
  General_script_def *specs_p_;
  bool postbreak_only_b_;
    
  static int compare (Script  *const&, Script *const&) ;
  Script();
  ~Script ();
  Script (Script const&);
   
  void set_stem (Stem*);
  
};


#endif // SCRIPT_HH

