/*
  general-script-def.hh -- declare General_script_def

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef GENERAL_SCRIPT_DEF_HH
#define GENERAL_SCRIPT_DEF_HH

#include "lily-proto.hh"
#include "input.hh"
#include "virtual-methods.hh"
#include "direction.hh"

/**
  Definition of anything that is put aside staff/notes.
 */
class General_script_def : public Input {
public:
  VIRTUAL_COPY_CONS(General_script_def,General_script_def);
  DECLARE_MY_RUNTIME_TYPEINFO;
  virtual Direction staff_dir() const;
  void print() const;
  virtual Direction rel_stem_dir() const;
  virtual int priority_i() const;
  virtual bool inside_b() const;
  virtual Atom get_atom (Paper_def* p, Direction dir_) const;
  bool equal_b (General_script_def const&) const;
  virtual ~General_script_def() {}
    
  virtual Interval width (Paper_def *) const;
protected:
  virtual bool do_equal_b (General_script_def const *) const;
  virtual void do_print() const;
};

#endif // GENERAL_SCRIPT_DEF_HH
