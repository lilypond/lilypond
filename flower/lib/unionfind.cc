#include "unionfind.hh"
/*
    see a book on data structures
    */

Union_find::Union_find(int n)
{
    classes.set_size(n);

    for (int i=0; i < n; i++) {
	classes[i] = i;
    }
}

int
Union_find::find(int i)
{
    int rep = i;
    while (classes[rep] != rep)
	rep = classes[rep];
    while (classes[i] != rep) {
	int next =classes[i];
	classes[i] = rep;
	i = next;
    }
    return rep;
}

void
Union_find::connect(int i, int j)
{
    i = find(i);
    j = find(j);
    classes[i] = j;    
}
