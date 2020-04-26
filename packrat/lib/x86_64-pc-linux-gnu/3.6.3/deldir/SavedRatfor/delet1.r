subroutine delet1(i,j,nadj,madj,ntot)
# Delete j from the adjacency list of i.
# Called by delet.

implicit double precision(a-h,o-z)
dimension nadj(-3:ntot,0:madj)

n    = nadj(i,0)
do k = 1,n {
        if(nadj(i,k)==j) { # Find j in the list;
                           # then move everything back one notch.
                do kk = k,n-1 { nadj(i,kk) = nadj(i,kk+1) }
                nadj(i,n) = -99 # Changed from the confusing 0 value 25/7/2011.
                nadj(i,0) = n-1
                return
        }
}

end
