#include <stdio.h>
#include "scalar.hh"

bool
Scalar::isnum()
{
    int conv = false;
    if (len()) {
	long l =0;
	conv = sscanf(data, "%ld", &l);
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
bool
Scalar::to_bool() const
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
