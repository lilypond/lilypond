#include "debug.hh"
 ostream &warnout (cerr);
 ostream *mlog(&cerr);



void warning(String s)
{
    WARN << s;
}


void error(String s)
{
    cerr << "\n" << s << "\nexiting..\n";
    exit(1);
}

