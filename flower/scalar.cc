#include <assert.h>
#include <stdio.h>
#include "scalar.hh"

Scalar::Scalar(Rational r)
    :String(r)
{

}

Scalar::operator Rational()
{
    int p = index_i('/');
    if (p == -1)
	return int(*this);
    
    String s2 = right_str(len()-p-1);
    String s1 = left_str(p);

    return Rational(s1.value_i(), s2.value_i());
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
    return value_f();
}

Scalar::operator int()
{
    assert (isnum());
    return value_i();
}


Scalar::operator bool() const
{
    if (!len())
	return false;
    if (*this == "0")
	return false;
    String u (*this);
    if ( u.upper_str() == "FALSE")
	return false;
    return true;
}
