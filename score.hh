#ifndef SCORE_HH
#define SCORE_HH

#include "vray.hh"
#include "cols.hh"
#include "mtime.hh"
#include "command.hh"

struct Score_column {
    PCol * pcol;
    svec<Mtime> durations;
    Mtime when;

    /// 
    bool musical;
    

    Score_column(Mtime when);

    static int compare(Score_column & c1, Score_column &c2) {
	return sgn(c1.when - c2.when);
    }
    void set_breakable() {
	 pcol->set_breakable();
    }
    bool used();
    void print() const;
};
/**

    When typesetting hasn't started on PScore yet, the columns which
    contain data have a rhythmical position. Score_column is the type
    with a rhythmical time attached to it. The calculation of
    idealspacing is done with data in these columns. (notably: the
    #durations# field)

    */

instantiate_compare(Score_column&, Score_column::compare);


/// the total music def of one movement
struct Score {
    String outfile;
    /// staffs_ and commands_ form the problem definition.
    PointerList<Staff *> staffs_;
    PointerList<Command*> commands_;
    
    /// "runtime" fields for setting up spacing    
    PointerList<Score_column*> cols_;
    PScore *pscore_;

    /****************************************************************/

    Score();
    
    /// add #Idealspacings# to #pscore_#
    void calc_idealspacing();
    void process();

    /// construction
    void add_staff(Staff *st);

    void distribute_commands();
    /** add the score wide commands (bars, breaks) to each staff so
    they can process (typeset) them if needed */
    void OK() const;
    Score_column *find_col(Mtime,bool);
    void do_pcols();
    void add(Command *);
    void add(Staff*);
    void add_command_seq(svec<Command*> );
    void output(String fn);
    PCursor<Score_column*> create_cols(Mtime);
    void print() const;
    void do_miscs() ;
    Mtime last() const;
    void clean_cols();
    void clean_commands();

    void do_connect(PCol *c1, PCol *c2, Real d);
    void connect_nonmus(PCol* c1, PCol *c2, Real d);
};
/**
        
    */
#endif
