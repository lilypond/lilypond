#include "translator.hh"

#include "translator-group.hh"
#include "lily-guile.hh"

LY_DEFINE(ly_get_context_property,
	  "ly-get-context-property", 2, 0, 0,
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
	  "ly-set-context-property", 3, 0, 0,
	  (SCM context, SCM name, SCM val),
	  "set value of property @var{name} in context @var{context} to @var{val}.
")
{
  Translator *t = unsmob_translator (context);
  Translator_group* tr=   dynamic_cast<Translator_group*> (t);

  SCM_ASSERT_TYPE(tr, context, SCM_ARG1, __FUNCTION__, "Context");
  tr->internal_set_property (name, val);

  return SCM_UNSPECIFIED;
}
