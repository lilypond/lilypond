/*
  ps-def.hh -- declare Ps_def

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <jannneke@gnu.org>
*/

#ifndef PS_DEF_HH
#define PS_DEF_HH

#include "paper-def.hh"

class Ps_def : public Paper_def
{
public:    
  VIRTUAL_COPY_CONS (Ps_def, Paper_def);
  DECLARE_MY_RUNTIME_TYPEINFO;

  virtual Lookup* lookup_p (Lookup const&) const;
  virtual Lookup* lookup_p (Symtables const&) const;

  virtual String output_settings_str () const;
  virtual Paper_score* paper_score_p () const;
  virtual String unknown_str () const;
};

#endif // PS_DEF_HH
