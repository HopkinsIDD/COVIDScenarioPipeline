subroutine locn(i,j,kj,nadj,madj,x,y,ntot,eps)

# Find the appropriate location for j in the adjacency list
# of i.  This is the index which j ***will*** have when
# it is inserted into the adjacency list of i in the
# appropriate place.  Called by insrt.

implicit double precision(a-h,o-z)
dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot)
logical before

n = nadj(i,0)

# If there is nothing already adjacent to i, then j will have place 1.
if(n==0) {
        kj = 1
        return
}

# Run through i's list, checking if j should come before each element
# of that list.  (I.e. if i, j, and k are in anti-clockwise order.)
# If j comes before the kj-th item, but not before the (kj-1)-st, then
# j should have place kj.
do ks = 1,n {
	kj = ks
        k = nadj(i,kj)
        call acchk(i,j,k,before,x,y,ntot,eps)
        if(before) {
                km = kj-1
                if(km==0) km = n
                k = nadj(i,km)
                call acchk(i,j,k,before,x,y,ntot,eps)
                if(before) next
                # If j is before 1 and after n, then it should
                # have place n+1.
                if(kj==1) kj = n+1
                return
        }
}

# We've gone right through the list and haven't been before
# the kj-th item ***and*** after the (kj-1)-st on any occasion.
# Therefore j is before everything (==> place 1) or after
# everything (==> place n+1).
if(before) kj = 1
else kj = n+1

return
end
