#ifndef REAL_HH
#define REAL_HH
typedef double Real;
inline Real sqr(Real x){
    return x*x;
}
inline Real MIN(Real x, Real y) {
    return (x < y)? x : y;
}

inline Real MAX(Real x, Real y) {
    return (x > y) ? x : y;
}

inline Real ABS(Real x)
{
    return (x>0)? x:-x;
}
inline
int sgn(Real x) {
    if (!x)return 0;
    return (x > 0) ?1: -1;
}
inline Real
distance(Real x,Real y)
{
    return ABS(x-y);
}
#endif
