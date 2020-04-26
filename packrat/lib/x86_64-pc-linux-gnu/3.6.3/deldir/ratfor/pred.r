subroutine pred(kpr,i,j,nadj,madj,ntot,nerror)

# Find the predecessor of j in the adjacency list of i.
# Called by initad, trifnd, swap, dirseg, dirout.

implicit double precision(a-h,o-z)
dimension nadj(-3:ntot,0:madj)

nerror = -1

n = nadj(i,0)

# If the adjacency list of i is empty, then clearly j has no predecessor
# in this adjacency list. Something's wrong; stop.
if(n==0) {
	nerror = 5
	return
}

# The adjacency list of i is non-empty; search through it until j is found;
# subtract 1 from the location of j, and find the contents of this new location
do k = 1,n {
        if(j==nadj(i,k)) {
                km = k-1
                if(km<1) km = n         # Take km modulo n. (The adjacency list
                kpr = nadj(i,km)        # is circular.)
                return
        }
}

# The adjacency list for i doesn't contain j.  Something's wrong; stop.
nerror = 6
return
end
