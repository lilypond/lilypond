/*
  curve.hh -- declare point and curve

  (c) 1998--1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef CURVE_HH
#define CURVE_HH

#ifndef STANDALONE
#include "lily-proto.hh"
#endif

#include "real.hh"

#include "offset.hh"
#include "array.hh"

class Curve : public Array<Offset>
{
public:
  void flipy ();
  int largest_disturbing ();
  void rotate (Real phi);
  void translate (Offset o);

  void operator = (Array<Offset> const & src) 
  {
    Array<Offset>::operator =(src);	
  }
  void operator = (Curve const & src) 
  {
    Array<Offset>::operator =((Array<Offset>)src);	
  }
};

#endif // CURVE_HH

