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
    Staff_column *get_col(Moment, PCursor<Staff_column*> * last= 0);
    Staff(const Staff&src);
    
    /// synchronous horizontal stuff
    IPointerList<Voice*> voice_list_;

public:

    /// runtime field
    IPointerList<Staff_column*> cols;

    Score *score_l_;
    PScore *pscore_l_;

    /* *************************************************************** */

    void add(const PointerList<Voice*> &s);

    void add_voice(Voice *v);
    Paperdef*paper()const;

    void setup_staffcols();

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
protected:
    virtual Staff_column * create_col()=0;
};
#endif
