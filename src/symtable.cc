#include "misc.hh"
#include "dimen.hh"
#include "debug.hh"
#include "real.hh"
#include "symbol.hh"
#include "assoc.hh"
#include "assoc-iter.hh"
#include "symtable.hh"

Symtables::Symtables()
{
}

Symtables::Symtables(Symtables const &s)
{
    for (Assoc_iter<String, Symtable*>  i(s); i.ok(); i++) {
	add(i.key(), new Symtable(*i.val()));
    }
}

Symtables::~Symtables()
{
    for (Assoc_iter<String, Symtable*>  i(*this); i.ok(); i++) {
	delete i.val();
    }
}

Symbol 
Symtable::lookup(String s) const
{
    if (elt_query(s))
	return (*this)[s];
    else {
	error( "Unknown symbol `" +s+"'\n");
    }
    Symbol sy;			// unreachable
    return sy;
}

Symtable* 
Symtables::operator()(String s) 
{
    return Assoc<String, Symtable*>::operator[](s);
} 
void
Symtables::print() const
{
    for (Assoc_iter<String, Symtable*>  i(*this); i.ok(); i++) {
	mtor << "table \'" << i.key() << "\' {\n";
	i.val()->print();
	mtor << "}\n";
    }
}
void
Symtable::print() const
{
    for (Assoc_iter<String, Symbol>  i(*this); i.ok(); i++) {
	mtor << "\'" << i.key() << "\'->" << i.val().str() << "\n";
    }
}
	
