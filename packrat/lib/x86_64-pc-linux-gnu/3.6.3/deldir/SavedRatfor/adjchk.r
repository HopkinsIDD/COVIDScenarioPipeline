subroutine adjchk(i,j,adj,nadj,madj,ntot,nerror)
# Check if vertices i and j are adjacent.
# Called by insrt, delet, trifnd, swap, delseg, dirseg.

dimension nadj(-3:ntot,0:madj)
logical adj

nerror = -1
# Check if j is in the adjacency list of i.
adj = .false.
ni  = nadj(i,0)
if(ni>0) {
        do k = 1,ni {
                if(j==nadj(i,k)) {
                        adj = .true.
                        break
                }
        }
}

# Check if i is in the adjacency list of j.
nj = nadj(j,0)
if(nj>0) {
        do k = 1,nj {
                if(i==nadj(j,k)) {
                        if(adj) return # Have j in i's list and i in j's.
                        else {
				nerror = 1
				return
                        }
                }
        }
}

# If we get to here i is not in j's list.
if(adj) { # If adj is true, then j IS in i's list.
	nerror = 1
	return
}

return
end
