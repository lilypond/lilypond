#include "key.hh"

const int OCTAVES=14;		// ugh..
const int ZEROOCTAVE=7;

Octave_key::Octave_key()
{
    accidental_i_arr_.set_size(7);
    for (int i= 0; i < 7 ; i++)
	accidental_i_arr_[i] = 0;
}

Key::Key()
{
    multi_octave_b_ = false;
    octaves.set_size(OCTAVES);
}

Octave_key&
Key::oct(int i)
{
    return octaves[i+ZEROOCTAVE];    
}

Octave_key
Key::oct(int i)const
{
    return octaves[i+ZEROOCTAVE];    
}

void
Octave_key::set(int i, int a)
{
    assert(a > -3 && a < 3);
    accidental_i_arr_[i]=a;    
}

void
Key::set(int o, int n , int a)
{
    octaves[o + ZEROOCTAVE].set(n,a);
}

void
Key::set (int n, int a)
{
    for (int i= 0; i < OCTAVES ; i++)
	octaves[i].set(n,a);
}
