subroutine swap(j,k1,k2,shdswp,nadj,madj,x,y,ntot,eps,nerror)

# The segment k1->k2 is a diagonal of a quadrilateral
# with a vertex at j (the point being added to the
# triangulation).  If the LOP is not satisfied, swap
# it for the other diagonal.
# Called by addpt.

implicit double precision(a-h,o-z)
dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot)
logical shdswp, anticl


# If vertices k1 and k2 are not connected there is no diagonal to swap.
# This could happen if vertices j, k1, and k2 were colinear, but shouldn't.
call adjchk(k1,k2,shdswp,nadj,madj,ntot,nerror)
if(nerror > 0) {
    return
}
if(!shdswp) return

# Get the other vertex of the quadrilateral.
call pred(k,k1,k2,nadj,madj,ntot,nerror) # If these aren't the same, then
if(nerror > 0) return
call succ(kk,k2,k1,nadj,madj,ntot,nerror) # there is no other vertex.
if(nerror > 0) return
if(kk!=k) {
        shdswp = .false.
        return
}

# Check whether the LOP is satisified; i.e. whether
# vertex k is outside the circumcircle of vertices j, k1, and k2
call qtest(j,k1,k,k2,shdswp,x,y,ntot,eps,nerror)
if(nerror > 0) return

# Do the actual swapping.
if(shdswp) {
        call delet(k1,k2,nadj,madj,ntot,nerror)
	if(nerror > 0) return
	call insrt(j,k,nadj,madj,x,y,ntot,nerror,eps)
	if(nerror > 0) return
}
return
end
