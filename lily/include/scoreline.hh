/*
  scoreline.hh -- part of GNU LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef SCORELINE_HH
#define SCORELINE_HH

#include "colhpos.hh"
#include "spanner-elem-group.hh"

/// the columns of a score that form one line.
class Line_of_score : public Spanner{
public:
    Link_array<PCol> cols;
    bool error_mark_b_;
    virtual String TeX_string() const;    
    
    DECLARE_MY_RUNTIME_TYPEINFO;
    Line_of_score();
    
    void add (Score_elem *);

    /// is #c# contained in #*this#?
    bool contains_b (PCol const *c)const;
    
    Link_array<Line_of_score> get_lines()const;
    void set_breaking (Array<Col_hpositions> const&);

protected:
    virtual void break_into_pieces (bool);
    virtual Interval do_width()const;
    virtual void do_print() const;
    SCORE_ELEM_CLONE(Line_of_score);
};

#endif

