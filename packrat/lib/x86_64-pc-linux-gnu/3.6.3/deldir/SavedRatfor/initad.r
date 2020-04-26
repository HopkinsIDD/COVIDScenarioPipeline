subroutine initad(j,nadj,madj,x,y,ntot,eps,ntri,nerror)

# Initial adding-in of a new point j.
# Called by addpt.

implicit double precision(a-h,o-z)
dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot)
integer tau(3)
integer tadj(1000)

# Find the triangle containing vertex j.
call trifnd(j,tau,nedge,nadj,madj,x,y,ntot,eps,ntri,nerror)
if(nerror > 0) return

# If the new point is on the edge of a triangle, detach the two
# vertices of that edge from each other.  Also join j to the vertex
# of the triangle on the reverse side of that edge from the `found'
# triangle (defined by tau) -- given that there ***is*** such a triangle.
if(nedge!=0) {
        ip = nedge
        i  = ip-1
        if(i==0) i = 3 # Arithmetic modulo 3.
        call pred(k,tau(i),tau(ip),nadj,madj,ntot,nerror)
	if(nerror > 0) return
        call succ(kk,tau(ip),tau(i),nadj,madj,ntot,nerror)
	if(nerror > 0) return
        call delet(tau(i),tau(ip),nadj,madj,ntot,nerror)
	if(nerror > 0) return
        if(k==kk) call insrt(j,k,nadj,madj,x,y,ntot,nerror,eps)
	if(nerror > 0) return
}

# Join the new point to each of the three vertices.
do i = 1,3 {
	call insrt(j,tau(i),nadj,madj,x,y,ntot,nerror,eps)
	if(nerror > 0) return
}
if(j==580) {
    nj = nadj(j,0)
    do jc = 1,nj {
        tadj(jc) = nadj(j,jc)
    }
    call intpr("Initial adjacency list of point 580:",-1,tadj,nj)
}

return
end
