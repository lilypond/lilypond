
/*
  axes.hh -- declare 

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef AXES_HH
#define AXES_HH

enum Axis {
    X_AXIS =0,
    Y_AXIS =1,
    NO_AXES=2,
};

class String;

String axis_name_str(Axis);

#endif // AXES_HH
