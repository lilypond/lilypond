#include "pstaff.hh"

struct Linestaff : PStaff {
    
    int nolines;

/****************/
    
    void brew_molecule(Real width);
    Linestaff(int, PScore*);   
};
