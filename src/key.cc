#include "key.hh"

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

Array<int>
Key::read(Array<Scalar> s)
{
    Array<int> newkey;
    for (int j = 0; j < 7; j++)
	accidentals[j] = 0;
   
    for (int i=0; i < s.size(); ) {
	int large = s[i++];
	int small = s[i++];
	accidentals[large]=small;

	newkey.add(large);
	newkey.add(small);
    }
    return newkey;
}

Array<int>
Key::oldkey_undo(Array<Scalar>s)
{
    Array<int> oldkey;
    Array<int> newkey;
    newkey.set_size(7);
    for (int i=0; i < newkey.size(); i++)
	newkey[i] = 0;
	
    for (int i=0; i < s.size(); ) {
	int large = s[i++];
	int small = s[i++];
	newkey[large] = small;
    }
    for (int i=0; i < newkey.size(); i++)
	if (accidentals[i] && accidentals[i] != newkey[i]) {
	    oldkey.add(i);
	    oldkey.add(0);
	}
    

    return oldkey;
}
