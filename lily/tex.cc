/*
  tex.cc -- implement TeX related misc functions

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "dimension.hh"
#include "tex.hh"
#include "atom.hh"
#include "array.hh"

String
vstrut (Real h)
{
  return String ("\\vrule height ") + print_dimen (h) + "depth 0pt width 0pt";
}


static void
substitute_arg (String& r, String arg)
{
  int p = r.index_i ('%');
  if (p < 0)
	return ;

  r = r.left_str (p) + arg + r.right_str (r.length_i() - p -1);
}


String
substitute_args (String source, Array<String> args)    
{
  String retval (source);
  for (int i = 0 ; i < args.size(); i++)
      substitute_arg (retval, args[i]);
  return retval;
}

String
substitute_args (String source, Array<Scalar> args)    
{
  Array<String> sv;
  for (int i = 0 ; i < args.size(); i++)
	sv.push (args[i]);
  
  return substitute_args (source, sv);
}
