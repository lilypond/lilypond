
#define iterator(set)		typeof((set).top())

// should use top()
#define iter_top(set,var)	iterator(set) var(set)

