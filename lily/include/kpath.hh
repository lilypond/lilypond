/*   
kpath.hh -- declare kpath funcs.

source file of the GNU LilyPond music typesetter

  (c)  2000--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#ifndef KPATH_HH
#define KPATH_HH



String kpathsea_find_afm (char const * name);
String kpathsea_find_tfm (char const * name);
void init_kpath (char *av0);



#endif /* KPATH_HH */

