#ifndef SCORE_HH
#define SCORE_HH
#include "vray.hh"
#include "proto.hh"
#include "plist.hh"


/// the total music def of one movement
struct Score {
    /// paper_, staffs_ and commands_ form the problem definition.
    Paperdef *paper_;
    IPointerList<Staff*> staffs_;

    svec<Command*> input_commands_;
    
    /// "runtime" fields for setting up spacing    
    IPointerList<Score_column*> cols_;
    PScore *pscore_;

    /****************************************************************/
    void add(svec<Command*> &s);
 
    /// construction
    void add_staff(Staff *st);
    void set(Paperdef*);
    Score();
    ~Score();    
    void add(Staff*);        



    void OK() const;
    PCursor<Score_column *> find_col(Real,bool);
    void process();
    void output(String fn);
    PCursor<Score_column*> create_cols(Real);
    void print() const;
    Real last() const;
    
private:
    void do_pcols();    
    void clean_cols();
    void distribute_commands();
    void do_connect(PCol *c1, PCol *c2, Real d,Real);
    void connect(PCol* c1, PCol *c2, Real d,Real = 1.0);
    
    /// add #Idealspacings# to #pscore_#
    void calc_idealspacing();
    /** add the score wide commands (bars, breaks) to each staff so
    they can process (typeset) them if needed */
};
/**
        
    */
#endif
