/*
  head-column.hh -- declare Head_column

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef HEAD_COLUMN_HH
#define HEAD_COLUMN_HH

#include "script-column.hh"

/**
  Scripts and rests/noteheads
 */
class Head_column : public Script_column
{ 
public:
  Link_array<Note_head> head_l_arr_;
  /** The relative position of the "voice" containing this
    chord. Normally this would be the same as the stem direction,
    but rests do not have stems.  

    Hmm. outdated.. Rests *do* have stems.
    */

  Direction dir_;
  Stem* stem_l_;

  void add (Note_head*);
  virtual void add (Script*s);
  void set (Stem*);
  Head_column();
  DECLARE_MY_RUNTIME_TYPEINFO;
protected:


  virtual void do_pre_processing();
  virtual void do_print() const;
  virtual void do_substitute_dependency (Score_elem*,Score_elem*);
};
#endif // HEAD_COLUMN_HH
