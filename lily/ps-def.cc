/*
  ps-def.cc -- implement Ps_def

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "assoc.hh"
#include "assoc-iter.hh"
#include "identifier.hh"
#include "ps-def.hh"
#include "ps-lookup.hh"
#include "ps-score.hh"
#include "scope.hh"

IMPLEMENT_IS_TYPE_B1 (Ps_def, Paper_def);

Lookup*
Ps_def::lookup_p (Lookup const& l) const
{
  return new Ps_lookup (l);
}

Lookup*
Ps_def::lookup_p (Symtables const& s) const
{
  return new Ps_lookup (s);
}

String
Ps_def::output_settings_str () const
{
  String s ("\n ");
  for (Assoc_iter<String,Identifier*> i (*scope_p_); i.ok (); i++)
    s += String ("/mudelapaper") + i.key ()
      + "{" + i.val ()->str () + "} bind def\n";
  s +=  *scope_p_->elem ("pssetting")->access_String ();
  return s;
}

Paper_score*
Ps_def::paper_score_p () const
{
  return new Ps_score ();
}
 
String
Ps_def::unknown_str () const
{
  return "unknown ";
}

