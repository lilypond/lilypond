#include "pstaff.hh"

struct Linestaff : PStaff {
    
    int nolines;

    Symbol get_stafsym(Real width)const;
    Linestaff(int);
    
};
