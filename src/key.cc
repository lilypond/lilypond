#include "key.hh"
#include "notename.hh"

const int OCTAVES=14;
const int ZEROOCTAVE=7;

Key::Key()
{
    accidentals.set_size(7);
    for (int i= 0; i < 7 ; i++)
	accidentals[i] = 0;
}

Local_key::Local_key()
{
    octaves.set_size(OCTAVES);
}

Key&
Local_key::oct(int i)
{
    return octaves[i+ZEROOCTAVE];    
}

void
Key::set(int i, int a)
{
    assert(a > -3 && a < 3);
    accidentals[i]=a;    
}


void
Local_key::reset(Key k)
{
    for (int i= 0; i < OCTAVES ; i++)
	octaves[i] = k;
}

svec<int>
Key::read(svec<String> s)
{
    svec<int> newkey;
    
    for (int i=0; i < s.sz(); i++) {
	int large, small;
	lookup_notename(large, small, s[i]);
	accidentals[large]=small;

	newkey.add(large);
	newkey.add(small);
    }
    return newkey;
}

svec<int>
Key::oldkey_undo(svec<String> s)
{
    svec<int> oldkey;
    svec<int> newkey;
    newkey.set_size(7);
    for (int i=0; i < newkey.sz(); i++)
	newkey[i] = 0;
	
    for (int i=0; i < s.sz(); i++) {
	int large, small;
	lookup_notename(large, small, s[i]);
	newkey[large] = small;
    }
    for (int i=0; i < newkey.sz(); i++)
	if (accidentals[i] && accidentals[i] != newkey[i]) {
	    oldkey.add(i);
	    oldkey.add(0);
	}
    

    return oldkey;
}
