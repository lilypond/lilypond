/*   
  symbol-cache.hh -- declare Symbol cacher.
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SYMBOL_CACHE_HH
#define SYMBOL_CACHE_HH

#if 1

/*
  A per file cache: for each compilation unit, there is a separate
  cache that maps the address of a string directly to a SCM value

 */
struct Symbol_cache_pair{
  const char * key;
  SCM val ;
};

static Symbol_cache_pair *private_symbol_cache;
static Symbol_cache_pair *private_symbol_cache_end;

static SCM
symbol (const char *ch) __attribute__ ((unused));

 SCM
symbol (const char *ch)
{
  Symbol_cache_pair * lo = private_symbol_cache;
  Symbol_cache_pair * hi = private_symbol_cache_end -1;

  if (lo)
    {
      do
	{
	  Symbol_cache_pair * mid = lo + (hi - lo) / 2 ;
	  if (mid->key > ch)
	    hi = mid;
	  else
	    lo = mid;
	}
      while ((hi - lo) > 1);
      if (lo->key== ch)
	return lo->val;
    }

  
  Symbol_cache_pair * p = private_symbol_cache;
  for (; p < private_symbol_cache_end
	 && p->key < ch ; p++)
    ;

  int idx = p - private_symbol_cache;
  
  SCM sym = gh_symbol2scm ((char*) ch);
  scm_permanent_object (sym);
  
  int sz = private_symbol_cache_end - private_symbol_cache;
  sz ++ ;
  private_symbol_cache
    = (Symbol_cache_pair*) realloc (private_symbol_cache,
				    sizeof (Symbol_cache_pair)* sz);
  private_symbol_cache_end = private_symbol_cache + sz;
  for (p = private_symbol_cache_end -1;
       p != private_symbol_cache + idx; p --)
    *p = *(p - 1);

  p->key = ch;
  p->val = sym;
    
  return sym;
}
#endif /* SYMBOL_CACHE_HH */
#endif
