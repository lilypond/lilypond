#include "glob.hh"
#include "string.hh"
#include "notename.hh"
#include "lexer.hh"
#include "identifier.hh"

    

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
/* *************** */
void
My_flex_lexer::set(Notename_tab *n)
{
    delete defaulttab;
    defaulttab = n;
}

void
My_flex_lexer::lookup_notename(int &large, int &small, String s)
{
    if (!defaulttab)
	set(lookup_identifier("default_table")->
	    notename_tab(true));
    
    defaulttab->lookup(large, small, s);
}
