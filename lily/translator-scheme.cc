/*   
  translator-scheme.cc --  implement Scheme context functions
 
  source file of the GNU LilyPond music typesetter

  (c) 2002--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#include "translator.hh"
#include "translator-def.hh"

#include "translator-group.hh"
#include "lily-guile.hh"

LY_DEFINE(ly_get_context_property,
	  "ly:get-context-property", 2, 0, 0,
	  (SCM context, SCM name),
	  "retrieve the value of @var{name} from context @var{context}")
{
  Translator *t = unsmob_translator (context);
  Translator_group* tr=   dynamic_cast<Translator_group*> (t);
  SCM_ASSERT_TYPE(tr, context, SCM_ARG1, __FUNCTION__, "Translator group");
  SCM_ASSERT_TYPE(gh_symbol_p (name), name, SCM_ARG2, __FUNCTION__, "symbol");

  return tr->internal_get_property (name);
  
}

LY_DEFINE(ly_set_context_property,
	  "ly:set-context-property!", 3, 0, 0,
	  (SCM context, SCM name, SCM val),
	  "set value of property @var{name} in context @var{context} to @var{val}.")
{
  Translator *t = unsmob_translator (context);
  Translator_group* tr=   dynamic_cast<Translator_group*> (t);

  SCM_ASSERT_TYPE(tr, context, SCM_ARG1, __FUNCTION__, "Context");
  SCM_ASSERT_TYPE(gh_symbol_p (name), name, SCM_ARG2, __FUNCTION__, "symbol");

  tr->internal_set_property (name, val);

  return SCM_UNSPECIFIED;
}


LY_DEFINE(ly_context_property_where_defined,
	  "ly:context-property-where-defined", 2, 0, 0,
	  (SCM context, SCM name),
	  "Return the context above @var{context} where @var{name}  is defined.")
{
  Translator *t = unsmob_translator (context);
  Translator_group* tr = dynamic_cast<Translator_group*> (t);
  SCM_ASSERT_TYPE(tr, context, SCM_ARG1, __FUNCTION__, "Context");
  SCM_ASSERT_TYPE(gh_symbol_p (name), name, SCM_ARG2, __FUNCTION__, "symbol");


  tr = tr->where_defined (name);

  if (tr)
    return tr->self_scm();

  return SCM_EOL;
}

LY_DEFINE(ly_unset_context_property,
	  "ly:unset-context-property", 2, 0, 0,
	  (SCM context, SCM name),
	  "Unset value of property @var{name} in context @var{context}.")
{
  Translator *t = unsmob_translator (context);
  Translator_group* tr = dynamic_cast<Translator_group*> (t);
  SCM_ASSERT_TYPE(tr, context, SCM_ARG1, __FUNCTION__, "Context");
  SCM_ASSERT_TYPE(gh_symbol_p (name), name, SCM_ARG2, __FUNCTION__, "symbol");

  tr->unset_property (name);

  return SCM_UNSPECIFIED;
}



LY_DEFINE(ly_context_parent,
	  "ly:context-parent", 1, 0, 0,
	  (SCM context),
	  "Return the parent of @var{context}, #f if none.")
{
  Translator *t = unsmob_translator (context);
  Translator_group* tr=   dynamic_cast<Translator_group*> (t);

  SCM_ASSERT_TYPE(tr, context, SCM_ARG1, __FUNCTION__, "Context");

  tr =  tr->daddy_trans_ ;
  if (tr)
    return tr->self_scm();
  else
    return SCM_BOOL_F;
}

/*
  Todo: should support translator IDs, and creation?
 */
LY_DEFINE(ly_translator_find,
	  "ly:translator-find", 2, 0,0,
	  (SCM context, SCM name),
	  "Find a parent of @var{context} that has name or alias @var{name}. "
	  "Return @code{#f} if not found." )
{
  Translator_group* tr=   dynamic_cast<Translator_group*> ( unsmob_translator (context));

  SCM_ASSERT_TYPE(tr, context, SCM_ARG1, __FUNCTION__, "context");
  SCM_ASSERT_TYPE(gh_string_p (name), name, SCM_ARG2, __FUNCTION__, "string");
  
  String s = ly_scm2string (name);
  
  while (tr)
    {
      if (tr->is_alias_b (s))
	return tr->self_scm();
      tr =  tr->daddy_trans_ ;
    }
  
  return SCM_BOOL_F;
}


LY_DEFINE(ly_context_properties,
	  "ly:context-properties", 1, 0, 0,
	  (SCM context),
	  "Return all properties  of @var{context} in an alist.")
{
  Translator *t = unsmob_translator (context);
  Translator_group* tr=   dynamic_cast<Translator_group*> (t);

  SCM_ASSERT_TYPE(tr, context, SCM_ARG1, __FUNCTION__, "Context");

  return tr->properties_as_alist ();
}



LY_DEFINE(ly_translator_name,
	  "ly:translator-name", 1,0,0,  (SCM trans),
	  "Return the type name of the translator @var{trans}.")
{
  Translator* tr =  unsmob_translator (trans);
  SCM_ASSERT_TYPE(tr, trans, SCM_ARG1, __FUNCTION__, "Context");

  char const* nm = classname (tr);
  return scm_makfrom0str (nm);
}

LY_DEFINE(ly_translator_description,
	  "ly:translator-description",
	  1,0,0, (SCM me),
	  "Return an alist of properties of  translator @var{me}.")
{
  Translator *tr =unsmob_translator (me);
  SCM_ASSERT_TYPE (tr, me, SCM_ARG1, __FUNCTION__, "Context");

  return tr->translator_description ();
}


LY_DEFINE(ly_context_pushpop_property,
	  "ly:context-pushpop-property", 3, 1, 0,
	  (SCM context, SCM grob, SCM eltprop, SCM val),
	  "Do a single @code{\\override} or @code{\\revert} operation "
	  "in @var{context}. The grob definition @code{grob} is extended with "
	  "@code{eltprop} (if @var{val} is specified) "
	  "or reverted (if  unspecified).")
{
  Translator_group *tg = dynamic_cast<Translator_group*> (unsmob_translator (context));

  SCM_ASSERT_TYPE(tg, context, SCM_ARG1, __FUNCTION__, "context");
  SCM_ASSERT_TYPE(gh_symbol_p (grob), grob, SCM_ARG2, __FUNCTION__, "symbol");
  SCM_ASSERT_TYPE(gh_symbol_p (eltprop), eltprop, SCM_ARG3, __FUNCTION__, "symbol");

  tg->execute_pushpop_property (grob, eltprop, val);

  return SCM_UNDEFINED;
}

LY_DEFINE(ly_context_p,
	  "ly:context?", 1, 0, 0,
	  (SCM x),
	  "Type predicate: is @var{x} a context?")
{
  Translator_group *tg = dynamic_cast<Translator_group*> (unsmob_translator (x));

  return SCM_BOOL (tg);
}
	  

int
Translator::print_smob (SCM s, SCM port, scm_print_state *)
{
  Translator *sc = (Translator *) ly_cdr (s);
     
  scm_puts ("#<Translator ", port);
  if (Translator_def *d=unsmob_translator_def (sc->definition_))
    {
      scm_display (d->type_name_, port);
    }
  else
    scm_display (ly_translator_name (s), port);

  scm_display (sc->simple_trans_list_, port);

  /*
    don't try to print properties, that is too much hassle.
   */
  scm_puts (" >", port);
  
  return 1;
}

