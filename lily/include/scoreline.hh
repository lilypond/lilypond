/*
  scoreline.hh -- part of GNU LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef SCORELINE_HH
#define SCORELINE_HH

#include "colhpos.hh"
#include "spanner.hh"

/// the columns of a score that form one line.
class Line_of_score : public Spanner 
{
public:
  Link_array<Paper_column> cols;
  bool error_mark_b_;

  DECLARE_MY_RUNTIME_TYPEINFO;
  Line_of_score();
    
  void add (Score_elem *);

  /// is #c# contained in #*this#?
  bool contains_b (Paper_column const *c) const;
    
  Link_array<Line_of_score> get_lines() const;
  void set_breaking (Array<Col_hpositions> const&);

protected:
  virtual Link_array<Score_elem> get_extra_dependencies () const;

  virtual void do_unlink ();
  virtual void do_junk_links ();
  virtual void break_into_pieces (bool);
  virtual Interval do_width() const;
  virtual void do_print() const;
  SCORE_ELEM_CLONE(Line_of_score);
};

#endif

