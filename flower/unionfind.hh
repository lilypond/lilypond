#ifndef UNIONFIND_HH
#define UNIONFIND_HH
#include "vray.hh"

/// which points of a graph are connected?
struct Union_find {    
    void connect(int i, int j);
    int find(int i);
    bool equiv(int i, int j) { return find(i) == find(j); }
    Union_find(int sz);

private:
    svec<int> classes;

};
/*
    Union find, a standard algorithm:

    Union_find represents an undirected graph of N points. You can
    connect two points using #connect()#. #find(i)# finds a uniquely
    determined representant of the equivalence class of points
    connected to #i#.
    
    */
#endif
