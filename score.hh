#ifndef SCORE_HH
#define SCORE_HH
#include "vray.hh"
#include "proto.hh"
#include "list.hh"


/// the total music def of one movement
struct Score {
    /// paper_, staffs_ and commands_ form the problem definition.
    Paperdef *paper_;
    PointerList<Staff *> staffs_;
    Score_commands *commands_;
    
    /// "runtime" fields for setting up spacing    
    PointerList<Score_column*> cols_;
    PScore *pscore_;

    /****************************************************************/

    /// construction
    void add_staff(Staff *st);
    void set(Paperdef*);
    Score();
    ~Score();    
    void add(Staff*);        
    void set(Score_commands*);


    void OK() const;
    Score_column *find_col(Real,bool);
    void process();
    void output(String fn);
    PCursor<Score_column*> create_cols(Real);
    void print() const;
    Real last() const;
    
private:
    void do_pcols();    
    void clean_cols();
    void distribute_commands();
    void do_connect(PCol *c1, PCol *c2, Real d);
    void connect_nonmus(PCol* c1, PCol *c2, Real d);
    /// add #Idealspacings# to #pscore_#
    void calc_idealspacing();
    /** add the score wide commands (bars, breaks) to each staff so
    they can process (typeset) them if needed */
};
/**
        
    */
#endif
