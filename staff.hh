#ifndef STAFF_HH
#define STAFF_HH

#include "score.hh"
#include "voice.hh"
#include "command.hh"

struct Staff_column {
    Score_column *score_column;

    /// fields to collect data vertically.
    svec<Voice_element *> v_elts;
    svec<Command *> s_commands;
    
    Staff_column(Score_column*s); 
    bool mus() const ;
    Mtime when() const;
    void add(Voice_element*ve);
    /****************************************************************
      VIRTUAL
    ****************************************************************/
    virtual void process_requests()=0;
    virtual void process_commands()=0;
    virtual ~Staff_column() { }
};


/// base class for a collection of voices.
struct Staff {
    /// synchronous horizontal stuff
    PointerList<Voice*> voices;

    /// commands in chronological order
    PointerList<Command *> commands;
    PointerList<Staff_column*> cols;

    /// indirections to the Score and PScore
    Score *score_;
    PScore *pscore_;

    void add_voice(Voice *v);
    void add_staff_column(Staff_column *sp);

	 
    /// interpret all requests and add items to #destination#.
    void process();
    /**
    This routines calls virtual functions from Staff, to delegate the
    interpretation of requests to a derived class of Staff */

    
    /****************************************************************
      VIRTUALS
    ****************************************************************/
    
    void setup_staffcols();

    virtual void set_output(PScore * destination)=0;
    virtual void grant_requests()=0;
    
    Staff_column * get_col(Mtime,bool);

    void add_commands(PointerList<Command* >const & sv);
    /**
    add all commands from sv.

    PRE
    sv is time-ordered.
    */
    virtual Staff_column * create_col(Score_column * )=0;

    void OK() const;
    void print() const;
    Mtime last() const;
    void clean_cols() ;
    virtual ~Staff() { } 
};
#endif
