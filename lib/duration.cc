/*
  duration.cc -- implement Duration, Plet, 

  source file of the LilyPond music typesetter

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>
           Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "proto.hh"
#include "plist.hh"
#include "string.hh"
#include "source-file.hh"
#include "source.hh"
#include "moment.hh"
#include "duration.hh"
#include "duration-convert.hh"

// statics Duration
int Duration::division_1_i_s = 384 * 4;


Duration::Duration()
{
	type_i_ = 1;
	dots_i_ = 0;
	ticks_i_ = 0;
}

bool
Duration::duration_type_b(int t)
{
    int bit_i=0;
    while (t > 0)
    {
	int rem = t % 2;
	t /= 2;
	bit_i += (rem == 1);
    }
    return bit_i == 1;
}

// ugh, what's this?
// i should be called "mom()", ... or at least "length_mom()"
Moment
Duration::length() const
{
    return Duration_convert::dur2_mom(*this);
}

void
Duration::set_plet(int i, int t)
{
    plet_.iso_i_ = i; 
    plet_.type_i_ = t;
}

void
Duration::set_plet(Duration d)
{
    plet_.iso_i_ = d.plet_.iso_i_; 
    plet_.type_i_ = d.plet_.type_i_;
}

void
Duration::set_ticks( int ticks_i )
{
	assert( !type_i_ );
	assert( !dots_i_ );
	ticks_i_ = ticks_i;
}

String
Duration::str()const
{
    return Duration_convert::dur2_str(*this);
}

Plet::Plet()
{
    type_i_ = 1;
    iso_i_ = 1;
}

Moment
Plet::mom()const
{
    return  Moment( iso_i_, type_i_ );
}

bool
Duration::plet_b()
{
    return !plet_.unit_b();
}

bool
Plet::unit_b()const
{
    return type_i_ == 1 && iso_i_ == 1;
}

