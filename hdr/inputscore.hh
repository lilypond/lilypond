#ifndef ISCORE_HH
#define ISCORE_HH
#include "varray.hh"
#include "proto.hh"
#include "plist.hh"
#include "string.hh"


/// the total music def of one movement
struct Input_score {
    /// defined where?    
    const char* defined_ch_c_l_;
    int errorlevel_i_;
    
    /// paper_, staffs_ and commands_ form the problem definition.
    Paperdef *paper_;
    IPointerList<Input_staff*> staffs_;
    IPointerList<Input_command*> commands_;
    
    /* *************************************************************** */
    Input_score();
    Input_score(Input_score const&);
    void add(Array<Input_command*> &s);
    void add(Input_staff*);
    ~Input_score();
    /// construction
    void set(Paperdef*);
    void print() const;
    Score*parse();
};

#endif
