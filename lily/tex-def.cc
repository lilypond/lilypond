/*
  tex-def.cc -- implement Tex_def

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "assoc.hh"
#include "assoc-iter.hh"
#include "identifier.hh"
#include "scope.hh"
#include "tex-def.hh"
#include "tex-lookup.hh"
#include "tex-score.hh"

IMPLEMENT_IS_TYPE_B1 (Tex_def, Paper_def);

String
Tex_def::dimension_str (Real r) const
{
  return Paper_def::dimension_str (r) + "pt";
}

Lookup*
Tex_def::lookup_p (Lookup const& l) const
{
  return new Tex_lookup (l);
}

Lookup*
Tex_def::lookup_p (Symtables const& s) const
{
  return new Tex_lookup (s);
}

String
Tex_def::output_settings_str () const
{
  String s ("\n ");
  for (Assoc_iter<String,Identifier*> i (*scope_p_); i.ok (); i++)
    s += String ("\\def\\mudelapaper") + i.key () 
      + "{" + i.val ()->str () + "}\n";
  s +=  *scope_p_->elem ("texsetting")->access_String ();
  return s;
}

Paper_score*
Tex_def::paper_score_p () const
{
  return new Tex_score ();
}
 
String
Tex_def::unknown_str () const
{
  return "\\unknown";
}

