/*
  script-def.hh -- part of GNU LilyPond

  (c) 1996--1998 Han-Wen Nienhuys
*/

#ifndef SCRIPTDEF_HH
#define SCRIPTDEF_HH
#include "string.hh"
#include "general-script-def.hh"

/** The characteristics of a certain kind of accent. It is not the
  accent itself.  */
class Script_def : public General_script_def {
  /// invert if below staff?
  bool invertsym_b_;
  String symidx_str_;
    

  /// on the other side of the stem?
  Direction rel_stem_dir_;

  /// below or above staff?
  Direction staff_dir_;

  /// how close to the note do we want to be?
  int priority_i_;
    
  /// follow the ball inside staff?
  bool inside_staff_b_;

public:
  virtual Direction staff_dir() const;
  virtual Direction rel_stem_dir() const;
  virtual int priority_i() const;
  virtual bool inside_b() const;
  virtual Molecule get_molecule (Paper_def* p, Direction dir_) const;
  

  virtual bool do_equal_b (General_script_def const *) const;
  virtual void do_print() const;
  Script_def();
  void set_from_input (String, bool, int, int ,bool,int);
protected:
  VIRTUAL_COPY_CONS(General_script_def);

};





#endif // SCRIPTDEF_HH

