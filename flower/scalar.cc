#include <assert.h>
#include <stdio.h>
#include "scalar.hh"

Scalar::Scalar(Rational r)
    :String(r)
{

}

Scalar::operator Rational()
{
    int p = pos('/');
    if (!p)
	return int(*this);
    
    String s2 = right(len()-p);
    p--;
    String s1 = left(p);

    return Rational(s1.value(), s2.value());
}

bool
Scalar::isnum()
{
    int conv = false;
    if (len()) {
	long l =0;
	conv = sscanf(strh_.ch_c_l(), "%ld", &l);
    }
    return len() && conv;
}

Scalar::operator Real()
{
    assert (isnum());
    return fvalue();
}

Scalar::operator int()
{
    assert (isnum());
    return value();
}


Scalar::operator bool() const
{
    if (!len())
	return false;
    if (*this == "0")
	return false;
    String u (*this);
    u.upper();
    if (u== "FALSE")
	return false;
    return true;
}
