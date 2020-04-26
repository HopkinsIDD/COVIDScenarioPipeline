subroutine swap(j,k1,k2,shdswp,nadj,madj,x,y,ntot,eps,nerror)

# The segment k1->k2 is a diagonal of a quadrilateral
# with a vertex at j (the point being added to the
# triangulation).  If the LOP is not satisfied, swap
# it for the other diagonal.
# Called by addpt.

implicit double precision(a-h,o-z)
dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot)
dimension ntadj(1000)
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
#    if(j==580) call intpr("no other vertex",-1,1,0)
        shdswp = .false.
        return
}

# Check whether the LOP is satisified; i.e. whether
# vertex k is outside the circumcircle of vertices j, k1, and k2
if(k==580) {
    call intpr("From swap; point being added =",-1,j,1)
# Adj. list of k1 ("now").
    nk1 = nadj(k1,0)
    do jc = 1,nk1 {
            ntadj(jc) = nadj(k1,jc)
    }
    call intpr("now =",-1,k1,1)
    call intpr("adjacency list of now:",-1,ntadj,nk1)
# Adj. list of k1 ("now").
    nk2 = nadj(k2,0)
    do jc = 1,nk2 {
            ntadj(jc) = nadj(k2,jc)
    }
    call intpr("nxt =",-1,k2,1)
    call intpr("adjacency list of nxt:",-1,ntadj,nk2)
# Adj. list of j ("point being added").
    nj = nadj(j,0)
    do jc = 1,nj {
            ntadj(jc) = nadj(j,jc)
    }
    call intpr("point being added =",-1,j,1)
    call intpr("adjacency list of point being added:",-1,ntadj,nj)
# j, now, nxt should be in anticlockwise order.
    call acchk(j,k1,k2,anticl,x,y,ntot,eps)
    if(anticl) {
        call intpr("anticlockwise",-1,1,0)
    } else {
        call intpr("clockwise",-1,1,0)
    }
#
# i = now = k1, k = nxt = k2, and j = other vertex = k:
}
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
