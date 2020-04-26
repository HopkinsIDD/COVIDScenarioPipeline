subroutine trifnd(j,tau,nedge,nadj,madj,x,y,ntot,eps,ntri,nerror)

# Find the triangle of the extant triangulation in which
# lies the point currently being added.
# Called by initad.

implicit double precision(a-h,o-z)
dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot), xt(3), yt(3)
integer tau(3)
logical adjace, anticl

nerror = -1

# The first point must be added to the triangulation before
# calling trifnd.
if(j==1) {
	nerror = 11
	return
}

# Get the previous triangle:
j1     = j-1
tau(1) = j1
tau(3) = nadj(j1,1)
call pred(tau(2),j1,tau(3),nadj,madj,ntot,nerror)
if(nerror > 0) return
call adjchk(tau(2),tau(3),adjace,nadj,madj,ntot,nerror)
if(nerror>0) {
    return
}
if(!adjace) {
        tau(3) = tau(2)
	call pred(tau(2),j1,tau(3),nadj,madj,ntot,nerror)
	if(nerror > 0) return
}

# Move to the adjacent triangle in the direction of the new
# point, until the new point lies in this triangle.
ktri = 0
1       continue

# Check that the vertices of the triangle listed in tau are
# in anticlockwise order.  (If they aren't then reverse the order;
# if they are *still*  not in anticlockwise order, theh alles
# upgefucken ist; throw an error.)
call acchk(tau(1),tau(2),tau(3),anticl,x,y,ntot,eps)
if(!anticl) {
    call acchk(tau(3),tau(2),tau(1),anticl,x,y,ntot,eps)
    if(!anticl) {
        call intpr("Point number =",-1,j,1)
        call intpr("Previous triangle:",-1,tau,3)
        call rexit("Both vertex orderings are clockwise. See help for deldir.")
    } else {
        ivtmp  = tau(3)
        tau(3) = tau(1)
        tau(1) = ivtmp
    }
}

ntau  = 0 # This number will identify the triangle to be moved to.
nedge = 0 # If the point lies on an edge, this number will identify that edge.
do i = 1,3 {
        ip = i+1
        if(ip==4) ip = 1 # Take addition modulo 3.

# Get the coordinates of the vertices of the current side,
# and of the point j which is being added:
	xt(1) = x(tau(i))
	yt(1) = y(tau(i))
	xt(2) = x(tau(ip))
	yt(2) = y(tau(ip))
	xt(3) = x(j)
	yt(3) = y(j)

# Create indicator telling which of tau(i), tau(ip), and j
# are ideal points.  (The point being added, j, is ***never*** ideal.)
	if(tau(i)<=0) i1 = 1
	else i1 = 0
	if(tau(ip)<=0) j1 = 1
	else j1 = 0
	k1 = 0
	ijk = i1*4+j1*2+k1

# Calculate the ``normalized'' cross product; if this is positive
# then the point being added is to the left (as we move along the
# edge in an anti-clockwise direction).  If the test value is positive
# for all three edges, then the point is inside the triangle.  Note
# that if the test value is very close to zero, we might get negative
# values for it on both sides of an edge, and hence go into an
# infinite loop.
	call cross(xt,yt,ijk,cprd)
	if(cprd >= eps) continue
	else if(cprd > -eps) nedge = ip
	else {
		ntau = ip
		break
	}
}

# We've played ring-around-the-triangle; now figure out the
# next move:

# case 0: All tests >= 0.; the point is inside; return.
if(ntau==0) {
if(j==580) {
   call intpr("Initial containing triangle for point 580:",-1,tau,3)
}
return
}

# The point is not inside; work out the vertices of the triangle to which
# to move.  Notation: Number the vertices of the current triangle from 1 to 3,
# anti-clockwise. Then "triangle i+1" is adjacent to the side from vertex i to
# vertex i+1, where i+1 is taken modulo 3 (i.e. "3+1 = 1").

# case 1: Move to "triangle 1"
if(ntau==1) {
	#tau(1) = tau(1)
	tau(2)  = tau(3)
	call succ(tau(3),tau(1),tau(2),nadj,madj,ntot,nerror)
	if(nerror > 0) return
}

# case 2: Move to "triangle 2"
if(ntau==2) {
	#tau(1) = tau(1)
	tau(3)  = tau(2)
	call pred(tau(2),tau(1),tau(3),nadj,madj,ntot,nerror)
	if(nerror > 0) return
}

# case 3: Move to "triangle 3"
if(ntau==3) {
	tau(1)  = tau(3)
	#tau(2) = tau(2)
	call succ(tau(3),tau(1),tau(2),nadj,madj,ntot,nerror)
	if(nerror > 0) return
}

# We've moved to a new triangle; check if the point being added lies
# inside this one.
ktri = ktri + 1
if(ktri > ntri) {
    call rexit("Cannot find an enclosing triangle.  See help for deldir.")
}
go to 1

end
