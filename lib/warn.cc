#include "warn.hh"
#include <stream.h>

void
error(String s)
{
    cerr <<  "error: " << s << "\n";
	
    exit(1);
}


void
warning(String m)
{
    cerr << "warning" <<m <<endl;

}

void
message(String m)
{
    cerr << m<<endl;
}
