#ifndef SCORE_HH
#define SCORE_HH
#include "varray.hh"
#include "proto.hh"
#include "plist.hh"
#include "moment.hh"
#include "assoc.hh"

/// the total music def of one movement
struct Score {
    /// paper_, staffs_ and commands_ form the problem definition.
    Paperdef *paper_p_;
    IPointerList<Staff*> staffs_;
    
    /// "runtime" fields for setting up spacing    
    IPointerList<Score_column*> cols_;
    PScore *pscore_p_;

    String define_spot_str_;
    
    Assoc<String, Moment> markers_assoc_;
    /****************************************************************/

    /// construction
    Score(Paperdef*);
    ~Score();    
    void add(Staff*);

    void add_marks(Array<String>, Array<Moment>);
    /// do everything except outputting to file
    void process();
    
    /// output to file
    void output(String fn);

    // standard
    void OK() const;
    void print() const;

    // utils:
    PCursor<Score_column*> create_cols(Moment);
    PCursor<Score_column *> find_col(Moment,bool);
    /// when does the last *musical* element finish?
    Moment last() const;

private:
    Score(Score const&){}
    ///
    void do_cols();
    /**
      make the pcol_l_ fields of each Score_column point to the correct PCol,
      remove any unnecessary Score_column's
     */

    /// remove unused cols
    void clean_cols();
    
    /// add #Idealspacings# to #pscore_#
    void calc_idealspacing();
    /** add the score wide commands (bars, breaks) to each staff so
    they can process (typeset) them if needed */
};
/**
        
    */
#endif
