/*
  note-column.hh -- declare Note_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef NOTE_COLUMN_HH
#define NOTE_COLUMN_HH

#include "item.hh"
#include "script-column.hh"

/** a struct for treating a group of noteheads (noteheads, stem
  (chord) and scripts) as a single entity.  */
class Note_column : public Script_column {
protected:
  virtual void do_pre_processing();
  virtual void do_print () const;
  virtual void do_substitute_dependency (Score_elem*,Score_elem*);
public:
  /** The relative position of the "voice" containing this
    chord. Normally this would be the same as the stem direction,
    but rests do not have stems.  
    */
  Direction dir_;
  bool h_shift_b_;
  Stem* stem_l_;


  Link_array<Note_head> head_l_arr_;
  Link_array<Rest> rest_l_arr_;
    
  Interval_t<int> head_positions_interval() const;
  Interval width () const;

  void translate_rests(int dy);
        
  DECLARE_MY_RUNTIME_TYPEINFO;
  Note_column ();
  void set (Stem*);
  void set (Dot_column*);
  void add (Rhythmic_head*);
  bool rest_b () const;
  virtual void add (Script*s);
  void sort ();
};

#endif // NOTE_COLUMN_HH
