#include "glob.hh"
#include "string.hh"
#include "notename.hh"
#include "lexer.hh"
#include "identifier.hh"

static Notename_tab * defaulttab = 0;

void
set_notename_tab(Notename_tab*n)
{
    delete defaulttab;
    defaulttab = n;
}

void
lookup_notename(int &large, int &small, String s)
{
    if (!defaulttab)
	set_notename_tab(lookup_identifier("default_table")->
			 notename_tab(true));
    
    defaulttab->lookup(large, small, s);
}
    

void
Notename_tab::lookup(int &large, int &small, String s)
{    
    large = -1;
    small = 0;

    for (int i =0; i < 7*5; i++)
	if (s == notetab[i]) 
	    {
	    large = i /5;
	    small = i %5 - 2;
	    return;	    
	    }
}


void
Notename_tab::set(int l, int s, String n)
{
    assert(l < 8 && s <= 2 && s >= -2 && l >=0);
    notetab[l * 5 + s +2] = n;
}
