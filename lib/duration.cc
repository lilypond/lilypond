/*
  duration.cc -- implement Duration, Plet, 

  source file of the LilyPond music typesetter

  copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
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


Duration::Duration( int type_i, int dots_i = 0)
{
// this breaks mi2mu quite effectively
//    assert(duration_type_b(type_i));
	type_i_ = type_i;
	dots_i_ = dots_i;
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

Plet::Plet( int iso_i, int type_i )
{
	iso_i_ = iso_i;
	type_i_ = type_i;
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

