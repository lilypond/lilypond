/*
  curve.hh -- declare point and curve

  (c) 1998 Jan Nieuwenhuizen <jan@digicash.com>
*/

#ifndef CURVE_HH
#define CURVE_HH

#ifndef STANDALONE
#include "lily-proto.hh"
#endif

#include "real.hh"

#include "offset.hh"
#include "varray.hh"

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

