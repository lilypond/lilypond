#include "glob.hh"
#include "string.hh"


/// change this along with lex file for other notenames.
const char *notetab[] = 
{
"ceses", "ces", "c", "cis", "cisis",
"deses", "des", "d", "dis", "disis",
"eses", "es", "e", "eis", "eisis",
"feses", "fes", "f", "fis", "fisis",
"geses", "ges", "g", "gis", "gisis",
"ases", "as", "a", "ais", "aisis",
"beses", "bes", "b", "bis", "bisis",
0
};

void
lookup_notename(int &large, int &small, String s)
{
    int i;
    for (i =0; notetab[i]; i++)
	if (s == notetab[i]) 
	    {
	    large = i /5;
	    small = i %5 - 2;
	    return;	    
	    }
    assert(false);    
}
