/*
  staff.hh -- declare Staff

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#ifndef STAFF_HH
#define STAFF_HH

#include "plist.hh"
#include "proto.hh"
#include "moment.hh"

/// base class for a collection of voices.
class Staff {
    Staff(const Staff&src);
    
public:
    Input_register * ireg_p_;
    
    Pointer_list<Voice*> voice_list_;
    /// runtime field
    Pointer_list<Staff_column*> cols_;

    Score *score_l_;
    PScore *pscore_l_;
    PStaff *pstaff_l_;
    
    /* *************************************************************** */

    void add(const Pointer_list<Voice*> &s);

    void add_voice(Voice *v_p);
    Paper_def*paper()const;

    void OK() const;
    void print() const;

    /// when does the last *musical* element finish?
    Moment last() const;

    /// remove unused cols
    void clean_cols() ;
    Staff();
    
    virtual void set_output(PScore * destination)=0;
    virtual Staff_walker *get_walker_p()=0;    
    virtual ~Staff() { }
    void add_col(Staff_column*);
protected:

};
#endif
