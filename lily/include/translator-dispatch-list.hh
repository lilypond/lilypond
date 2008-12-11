/*
  translator-dispatch-list.hh -- declare Translator_dispatch_list

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef TRANSLATOR_DISPATCH_LIST_HH
#define TRANSLATOR_DISPATCH_LIST_HH

#include "lily-proto.hh"
#include "std-vector.hh"
#include "smobs.hh"

struct Engraver_dispatch_entry
{
  Engraver *engraver_;
  Engraver_void_function_engraver_grob_info function_;
};

class Engraver_dispatch_list
{
  vector<Engraver_dispatch_entry> dispatch_entries_;
public:
  void apply (Grob_info);
  SCM static create (SCM trans_list,
		     SCM iface_list, Direction);

  DECLARE_SIMPLE_SMOBS (Engraver_dispatch_list);
};

#endif /* TRANSLATOR_DISPATCH_LIST_HH */
