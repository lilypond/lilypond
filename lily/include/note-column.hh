/*
  note-column.hh -- declare Note_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  virtual void do_substitute_element_pointer (Score_element*,Score_element*);
public:
  /** The relative position of the "voice" containing this
    chord. Normally this would be the same as the stem direction,
    but rests do not have stems.

    JUNKME.v
    */
  Direction dir_;
  Stem* stem_l_;


  Link_array<Note_head> head_l_arr_;
  Link_array<Rest> rest_l_arr_;
    
  Interval_t<int> head_positions_interval() const;
  //  Interval width () const;

  void translate_rests(int dy);
        
  
  Note_column ();
  void set_stem (Stem*);
  void set_dotcol (Dot_column*);
  void add_head (Rhythmic_head*);
  bool rest_b () const;
  virtual void add_script (Script*s);
  void sort ();
};

#endif // NOTE_COLUMN_HH
