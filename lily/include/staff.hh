/*
  staff.hh -- declare Staff

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#ifndef STAFF_HH
#define STAFF_HH

#include "plist.hh"
#include "lily-proto.hh"
#include "moment.hh"

/// A collection of voices.
class Staff {
    Staff(const Staff&src);
    
public:
    Input_register * ireg_p_;
    
    Link_list<Voice*> voice_list_;
    /// runtime field
    Link_list<Staff_column*> cols_;
    Line_of_staff * staff_line_l_;

    Score *score_l_;
    PScore *pscore_l_;
    
    /* *************************************************************** */

    void add(Link_list<Voice*> const&s);

    void add_voice(Voice *v_p);
    Paper_def*paper()const;

    void OK() const;
    void print() const;

    /// when does the last *musical* element finish?
    Moment last() const;

    /// remove unused cols
    void clean_cols() ;
    Staff();
    
    virtual void set_output(PScore * destination);
    Staff_walker *get_walker_p();
    virtual ~Staff();
    void add_col(Staff_column*);
protected:

};
#endif
