#ifndef STAFF_HH
#define STAFF_HH

#include "staffcommands.hh"


/// base class for a collection of voices.
struct Staff {
    /// synchronous horizontal stuff
    IPointerList<Voice*> voices;

    /// commands in chronological order
    Staff_commands *staff_commands_;
    
    /// runtime field
    IPointerList<Staff_column*> cols;

    /// indirections to the Score and PScore
    Score *score_;
    PScore *pscore_;

    /****************************************************************/
    void add(PointerList<Voice*> &s);
    void process_commands(Moment l);

    Staff(const Staff&src);
    void add_voice(Voice *v);
    void add_staff_column(Staff_column *sp);

    Paperdef*paper()const;
    /// interpret all requests and add items to #destination#.
    void process();
    /**
    This routines calls virtual functions from Staff, to delegate the
    interpretation of requests to a derived class of Staff */
    void setup_staffcols();

    void OK() const;
    void print() const;
    Moment last() const;
    void clean_cols() ;
    Staff_column * get_col(Moment,bool);

    Staff();

    /****************************************************************
      VIRTUALS
    ****************************************************************/

    virtual void set_output(PScore * destination)=0;
    virtual void walk()=0;    
    virtual Staff_column * create_col(Score_column * )=0;
    virtual ~Staff() { }
};
#endif


