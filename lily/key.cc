#include "key.hh"

const int OCTAVES=14;		// ugh..
const int ZEROOCTAVE=7;

Key::Key()
{
    accidental_i_arr_.set_size(7);
    for (int i= 0; i < 7 ; i++)
	accidental_i_arr_[i] = 0;
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
    accidental_i_arr_[i]=a;    
}


void
Local_key::reset(Key k)
{
    for (int i= 0; i < OCTAVES ; i++)
	octaves[i] = k;
}

