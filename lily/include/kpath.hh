/*   
kpath.hh -- declare kpath funcs.

source file of the GNU LilyPond music typesetter

  (c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#ifndef KPATH_HH
#define KPATH_HH



char * ly_find_afm (char const * name);
String ly_find_tfm (char const * name);
void ly_init_kpath (char *av0);



#endif /* KPATH_HH */

