#ifndef SCORE_HH
#define SCORE_HH

#include "vray.hh"
#include "mtime.hh"
#include "scommands.hh"

/// the total music def of one movement
struct Score {
    Paperdef *paper;
    /// staffs_ and commands_ form the problem definition.
    PointerList<Staff *> staffs_;
    Score_commands commands_;
    
    /// "runtime" fields for setting up spacing    
    PointerList<Score_column*> cols_;
    PScore *pscore_;

    /****************************************************************/

    Score();    
    void process();

    /// construction
    void add_staff(Staff *st);

    void OK() const;
    Score_column *find_col(Mtime,bool);
    void do_pcols();

    void add(Staff*);
    void output(String fn);
    PCursor<Score_column*> create_cols(Mtime);
    void print() const;

    Mtime last() const;
    
    void add(Command*);
    
private:
    
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
