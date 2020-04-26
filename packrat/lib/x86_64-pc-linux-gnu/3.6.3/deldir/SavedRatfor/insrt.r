subroutine insrt(i,j,nadj,madj,x,y,ntot,nerror,eps)
# Insert i and j into each other's adjacency list.
# Called by master, initad, swap.

implicit double precision(a-h,o-z)
dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot)
logical adj

# Check whether i and j are in each other's adjacency lists.
call adjchk(i,j,adj,nadj,madj,ntot,nerror)
if(nerror > 0) {
    return
}
if(adj) return

# If not, find where in each list they should respectively be.
call locn(i,j,kj,nadj,madj,x,y,ntot,eps)
call locn(j,i,ki,nadj,madj,x,y,ntot,eps)

# Put them in each other's lists in the appropriate position.
call insrt1(i,j,kj,nadj,madj,ntot,nerror)
if(nerror >0) return
call insrt1(j,i,ki,nadj,madj,ntot,nerror)
if(nerror >0) return

return
end
