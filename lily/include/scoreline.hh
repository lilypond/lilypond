
/*
  scoreline.hh -- part of GNU LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef SCORELINE_HH
#define SCORELINE_HH

#include "colhpos.hh"
#include "spanner-elem-group.hh"

/// the columns of a score that form one line.
class Line_of_score : public Spanner_elem_group {
public:
    Link_array<Spanner_elem_group> line_arr_;
    Link_array<PCol > cols;
    bool error_mark_b_;
    virtual String TeX_string() const;    
    
    /* *************** */
    NAME_MEMBERS();
    Line_of_score();
    
    void add_line(Spanner_elem_group *);

    /// is #c# contained in #*this#?
    bool contains_b(PCol const *c)const;
    
    Link_array<Line_of_score> get_lines()const;
    void set_breaking(Array<Col_hpositions> const&);
    
protected:
    virtual void break_into_pieces();
    virtual void do_substitute_dependency(Score_elem*,Score_elem*);
    virtual void do_pre_processing();
    virtual void do_post_processing();


    SPANNER_CLONE(Line_of_score)
};

#endif

