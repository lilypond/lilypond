#ifndef STAFF_HH
#define STAFF_HH

#include "staffcommands.hh"


/// base class for a collection of voices.
struct Staff {
    /// synchronous horizontal stuff
    IPointerList<Voice*> voices;

    /// runtime field
    IPointerList<Staff_column*> cols;

    Score *score_l_;
    PScore *pscore_l_;
    String define_spot_str_;

    /* *************************************************************** */

    void add(PointerList<Voice*> &s);
    void do_commands(PointerList<Input_command*> score_wide,
		     PointerList<Input_command*> staff_wide);

    void get_marks(Array<String>&, Array<Moment>&);
    
    /// throw away cols later the #l#
    void truncate_cols(Moment l);

    Staff(const Staff&src);
    void add_voice(Voice *v);
    void add_staff_column(Staff_column *sp);

    Paperdef*paper()const;
    
    /// interpret all requests and add items to #destination#.
    /**
    This routines calls virtual functions from Staff, to delegate the
    interpretation of requests to a derived class of Staff */
    void process();

    
    void setup_staffcols();

    void OK() const;
    void print() const;

    /// when does the last *musical* element finish?
    Moment last() const;

    /// remove unused cols
    void clean_cols() ;
    
    Staff_column *get_col(Moment,bool); // ugh

    Staff();

    /* 
      VIRTUALS
      */

    virtual void set_output(PScore * destination)=0;
    virtual void walk()=0;    
    virtual Staff_column * create_col(Score_column * )=0;
    virtual ~Staff() { }
private:
    void set_time_descriptions();
};
#endif


