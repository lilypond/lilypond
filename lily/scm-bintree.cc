/*   
  scm-bintree.cc --  implement binary trees, an experiment in GC.
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include <stdio.h>
#include "libc-extension.hh"
#include "lily-guile.hh"
#include "main.hh"
#include "debug.hh"




/*
  Layout of nodes:

  (key . (left_child . right_child))

  SCM_EOL is the nil-pointer (should use SCM_NIMP() ?)
 */

#define left_child(s) SCM_CADR((s))
#define right_child(s) SCM_CDDR((s))
#define key(s) SCM_CAR((s))

/*
  Garble pointers, to prevent unbalanced tree due to ordered inserts.
 */

unsigned int
munge (SCM s) 
{
  const int SHIFT = 18;
  return (unsigned int)(s << (32-SHIFT) | s >> SHIFT );
}

SCM
ly_new_bintree_node (SCM val)
{
  return gh_cons (val, gh_cons (SCM_EOL, SCM_EOL));
}


/*
  add VAL to TREE. TREE must be non-nil
 */
void
ly_addto_bintree (SCM *tree, SCM val)
{
  while(*tree != SCM_EOL)
    {
      if (munge (val) <= munge (key (*tree)))
	tree = &left_child (*tree);
      else
	tree = &right_child (*tree);
    }

  *tree = ly_new_bintree_node (val);
}


/*
  find the address of a node in the tree represented by *NODE with key VAL
 */
SCM  *
ly_find_in_bintree (SCM *node, SCM val)
{
  while (*node != SCM_EOL)
    {
      if (munge (val) < munge (key(*node) ))
	node = &left_child(*node);
      else if (munge (val) > munge (key (*node)))
	node = &right_child (*node);
      else
	return node;
    }
  return node;
}

void
ly_remove_from_bintree (SCM *node)
{
  SCM r = right_child  (*node);
  SCM l = left_child (*node);
  
  if (r == SCM_EOL)
    {
      *node = l;
    }
  else if (l == SCM_EOL)
    {
      *node = r;
    }
  else
    {
      /*deleting from binary trees.  See Knuth's TAOCP.
       */
      SCM *t = node;
      SCM *left_t = &left_child (*t);

      /*
	INV:  LEFT_T  is the left child of T
       */
      while (*left_t != SCM_EOL)
	{
	  t = left_t;
	  left_t = &left_child (*t);
	}

      /*
	POST: T is the leftmost right child of NODE which has no left child,

	leftchild (LASTT) == T
       */
      key(*node) = key(*t);
      *left_t = right_child (*t);
    }
}


static SCM protect_tree_root;

SCM
ly_protect_scm (SCM s)
{
  ly_addto_bintree (&protect_tree_root, s);
  return s;
}

SCM
ly_unprotect_scm (SCM s)
{
  SCM *to_remove = ly_find_in_bintree (&protect_tree_root, s);

  /*
    this shouldn't happen, according to me. But it does.
   */
  if (*to_remove != SCM_EOL)
    ly_remove_from_bintree (to_remove);
  return s;
}

void
ly_init_protection ()
{
  protect_tree_root = scm_protect_object (ly_new_bintree_node(SCM_EOL));
  key (protect_tree_root) = protect_tree_root;
}


int
ly_count_elements (SCM tree)
{
  if (tree == SCM_EOL)
    return 0;
  else
    return 1 + ly_count_elements (left_child (tree)) + ly_count_elements (right_child( tree));
}

int
ly_tree_depth (SCM tree)
{
  if (tree == SCM_EOL)
    return 0;
  else
    return 1 + (ly_tree_depth (left_child (tree)) >? ly_tree_depth (right_child(tree)));
}

void
ly_print_bintree (SCM node)
{
#ifndef NPRINT
  if (node == SCM_EOL)
    return;
  DOUT << "{val = " << key(node) << " \nleft = ";
  ly_print_bintree (left_child (node));
  DOUT << "\n right =";
  ly_print_bintree (right_child (node));
  DOUT << "}";
#endif
}


struct Imbalance { int imbalance; int total; };

Imbalance
ly_calc_imbalance (SCM node)
{
  Imbalance t;
  if (node == SCM_EOL)
    {
      t.imbalance = 0;
      t.total = 0;
      return t;
    }

  Imbalance l = ly_calc_imbalance (left_child (node));
  Imbalance r = ly_calc_imbalance (right_child (node));

  t.total = l.total + r.total + 1;
  int dif = l.total - r.total;
  if (dif < 0)
     dif = -dif;
  t.imbalance = l.imbalance + r.imbalance + dif;
  return t;
}

