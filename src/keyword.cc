/*
  keyword.cc -- keywords and identifiers
 */

#include <stdlib.h>

#include "glob.hh"
#include "lexer.hh"
//#include "mudobs.hh"
//#include "gram.hh"

/* for the keyword table */
struct Keyword_ent
{
    const char   *name;
    int     tokcode;
};

struct Keyword_table
{
    Keyword_ent *table;
    int     maxkey;
    Keyword_table(Keyword_ent *);
    int     lookup(const char *s) const;
};


/* for qsort */
int
        tabcmp(const void * p1, const void * p2)
{
    return strcmp(((const Keyword_ent *) p1)->name,
                  ((const Keyword_ent *) p2)->name);
}

Keyword_table::Keyword_table(Keyword_ent *tab)
{
    table = tab;

    /* count keywords */
    for (maxkey = 0; table[maxkey].name; maxkey++);

    /* sort them */
    qsort(table, maxkey, sizeof(Keyword_ent), tabcmp);
}

/*
  lookup with binsearch, return tokencode.
*/
int
Keyword_table::lookup(const char *s)const
{
    int     lo,
            hi,
            cmp,
            result;
    lo = 0;
    hi = maxkey;

    /* binary search */
    do
    {
        cmp = (lo + hi) / 2;

        result = strcmp(s, table[cmp].name);

        if (result < 0)
            hi = cmp;
        else
            lo = cmp;
    }
    while (hi - lo > 1);
    if (!strcmp(s, table[lo].name))
    {
        return table[lo].tokcode;
    } else
        return -1;              /* not found */
}

