subroutine insrt1(i,j,kj,nadj,madj,ntot,nerror)

# Insert j into the adjacency list of i.
# Called by insrt.

implicit double precision(a-h,o-z)
dimension nadj(-3:ntot,0:madj)

nerror = -1

# Variable  kj is the index which j ***will***
# have when it is inserted into the adjacency list of i in
# the appropriate position.

# If the adjacency list of i had no points just stick j into the list.
n = nadj(i,0)
if(n==0) {
        nadj(i,0) = 1
        nadj(i,1) = j
        return
}

# If the adjacency list had some points, move everything ahead of the
# kj-th place one place forward, and put j in position kj.
kk = n+1

if(kk>madj) { # Watch out for over-writing!!!
	nerror = 4
	return
}
while(kk>kj) {
        nadj(i,kk) = nadj(i,kk-1)
        kk = kk-1
}
nadj(i,kj) = j
nadj(i,0)  = n+1

return
end
