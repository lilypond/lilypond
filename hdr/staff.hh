#ifndef STAFF_HH
#define STAFF_HH

#include "score.hh"
#include "voice.hh"
#include "command.hh"


/// base class for a collection of voices.
struct Staff {
    /// synchronous horizontal stuff
    PointerList<Voice*> voices;

    /// commands in chronological order
    PointerList<Command *> commands;

    /// runtime field
    PointerList<Staff_column*> cols;

    /// indirections to the Score and PScore
    Score *score_;
    PScore *pscore_;

    /****************************************************************/
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
    Real last() const;
    void clean_cols() ;
    Staff_column * get_col(Real,bool);

    void add_commands(PointerList<Command* >const & sv);
    /**
    add all commands from sv.

    PRE
    sv is time-ordered.
    */

    Staff();
    /**
      Should construct with Score as arg, but this isn't known during parsing.      
      */
    /****************************************************************
      VIRTUALS
    ****************************************************************/
    virtual Staff*clone()const=0;    
    virtual void set_output(PScore * destination)=0;
    virtual void walk()=0;    
    virtual Staff_column * create_col(Score_column * )=0;
    virtual ~Staff() { }
};
#endif


