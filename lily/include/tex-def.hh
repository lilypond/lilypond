/*
  tex-def.hh -- declare Tex_def

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <jannneke@gnu.org>
*/

#ifndef TEX_DEF_HH
#define TEX_DEF_HH

#include "paper-def.hh"

class Tex_def : public Paper_def
{
public:    
  VIRTUAL_COPY_CONS (Tex_def, Paper_def);
  DECLARE_MY_RUNTIME_TYPEINFO;

  virtual String dimension_str (Real r) const;
  virtual Lookup* lookup_p (Lookup const&) const;
  virtual Lookup* lookup_p (Symtables const&) const;

  virtual String output_settings_str () const;
  virtual Paper_score* paper_score_p () const;
  virtual String unknown_str () const;
};

#endif // TEX_DEF_HH
