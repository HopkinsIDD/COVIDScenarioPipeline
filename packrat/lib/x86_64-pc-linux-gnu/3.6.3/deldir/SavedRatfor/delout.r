subroutine delout(delsum,nadj,madj,x,y,ntot,npd,nerror)

# Put a summary of the Delaunay triangles with a vertex at point i,
# for i = 1, ..., npd, into the array delsum.  Do this in the original
# order of the points, not the order into which they have been
# bin-sorted.
# Called by master.

implicit double precision(a-h,o-z)
dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot)
dimension delsum(npd,4)

do i = 1,npd {
        area = 0.   # Initialize area of polygon consisting of triangles
                    # with a vertex at point i.
        # Get the coordinates of the point and the number of
        # (real) triangles emanating from it.
        np = nadj(i,0)
	xi = x(i)
	yi = y(i)
        npt = np
        do k = 1,np {
                kp = k+1
                if(kp>np) kp = 1
                if(nadj(i,k)<=0|nadj(i,kp)<=0) npt = npt-1
        }

        # For each point in the adjacency list of point i, find its
        # successor, and the area of the triangle determined by these
        # three points.
        do j1 = 1,np {
                j = nadj(i,j1)
                if(j<=0) next
		xj = x(j)
		yj = y(j)
                call succ(k,i,j,nadj,madj,ntot,nerror)
		if(nerror > 0) return
                if(k<=0) next
		xk = x(k)
		yk = y(k)
                call triar(xi,yi,xj,yj,xk,yk,tmp)
                # Downweight the area by 1/3, since each
                # triangle eventually appears 3 times over.
                area = area+tmp/3.
        }
	delsum(i,1) = xi
	delsum(i,2) = yi
	delsum(i,3) = npt
	delsum(i,4) = area
}

return
end
