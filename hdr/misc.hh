#ifndef MISC_HH
#define MISC_HH

#include "real.hh"
#include "moment.hh"

Moment wholes(int dur, int dots);
    
double log_2(double x) ;
int intlog2(int d);
inline int
abs(int i){
    return (i < 0)?-i:i;
}
inline int
sign(int i) {
    if (i<0) return -1;
    else if (i) return 1;
    else return 0;
}


#endif
