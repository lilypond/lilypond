#include <ctype.h>
#include "dimen.hh"
#include "debug.hh"
#include "string.hh"

Real
parse_dimen(String dim)
{
    int i=dim.len()-1;
    const char *s = dim;
    while  (i > 0 && (isspace(s[i]) || isalpha(s[i])) ){
	i--;
    }
    String unit(s + i+1);
    return convert_dimen(dim.fvalue(), unit); 
}

const Real CM_TO_PT=72/2.54;

Real
convert_dimen(Real quant, String unit)
{
    if (unit == "cm")
	return quant * CM_TO_PT;
    if (unit == "pt")
	return quant;
    if (unit == "mm")
	return quant*CM_TO_PT/10;
    if (unit == "in")
	return quant * 72;
    error ("unknown length unit: `" + unit+"'");
}

String
print_dimen(Real r)
{
    String s(r);
    s += "pt ";
    return s;
}
