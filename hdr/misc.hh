#ifndef MISC_HH
#define MISC_HH

#include "real.hh"

Real wholes(int dur, int dots);
    
double log_2(double x) ;
int intlog2(int d);
inline int
ABS(int i)
{
    return (i < 0)?-i:i;
}


#endif
