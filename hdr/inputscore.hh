#ifndef ISCORE_HH
#define ISCORE_HH
#include "vray.hh"
#include "proto.hh"
#include "plist.hh"


/// the total music def of one movement
struct Input_score {
    /// paper_, staffs_ and commands_ form the problem definition.
    Paperdef *paper_;
    IPointerList<Input_staff*> staffs_;
    IPointerList<Input_command*> commands_;
    
    /****************************************************************/
    Input_score();
    Input_score(Input_score&);
    void add(svec<Input_command*> &s);
    void add(Input_staff*);
    ~Input_score();
    /// construction
    void set(Paperdef*);
    void print() const;
    Score*parse();
};
/**
        
    */
#endif
