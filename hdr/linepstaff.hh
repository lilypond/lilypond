#include "pstaff.hh"

struct Linestaff : PStaff {
    
    int nolines;

/****************/
    
    void brew_molecule_p(Real width);
    Linestaff(int, PScore*);   
};
