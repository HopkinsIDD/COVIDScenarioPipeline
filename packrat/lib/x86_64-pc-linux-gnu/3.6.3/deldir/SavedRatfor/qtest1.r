subroutine qtest1(h,i,j,k,x,y,ntot,eps,shdswp,nerror)

# The Lee-Schacter test for the LOP (all points are real,
# i.e. non-ideal).  If the LOP is ***not*** satisfied (i.e. if
# vertex j is inside the circumcircle of vertices h, i, and k) then the
# diagonals should be swapped, i.e. shdswp ("should-swap") is true.
# Called by qtest.

implicit double precision(a-h,o-z)
dimension x(-3:ntot), y(-3:ntot), xt(3), yt(3), indv(3)
integer h
logical shdswp, collin

# The vertices of the quadrilateral are labelled
# h, i, j, k in the anticlockwise direction, h
# being the point of central interest.

# Make sure the quadrilateral is convex, so that
# it makes sense to swap the diagonal.
# call acchk(i,j,k,shdswp,x,y,ntot,eps)
# if(!shdswp) return
#
# 23 July 2011:
# The foregoing test is a load of dingoes' kidneys.  (1) It is
# unnecessary, and (2) it is wrong!  (1) If the LOP is not satisfied
# (the only circumstance under which there should be a swap) then the
# quadrilateral ***must*** be convex, and so swapping can sensibly
# take place.  (2) The vertices i, j, k in will ***always*** be in
# anticlockwise order, since the vertices h, i, j, k of the quadrilateral
# are in such order and i is connected to k, whence j can't be inside
# the triangle ihk.  So the test does nothing.  But then it didn't need
# to do anything.

# Check for collinearity of points h, i and k.
xt(1) = x(h)
yt(1) = y(h)
xt(2) = x(i)
yt(2) = y(i)
xt(3) = x(k)
yt(3) = y(k)
nid = 0  # nid = number of ideal points.
call cross(xt,yt,nid,cprd)
collin = (abs(cprd) < eps) # Does this work???

# If the points are collinear, make sure that they're in the right
# order --- h between i and k.
if(collin) {
# Form the vector u from h to i, and the vector v from h to k,
# and normalize them.
    a  = xt(2) - xt(1)
    b  = yt(2) - yt(1)
    c  = xt(3) - xt(1)
    d  = yt(3) - yt(1)
    c1 = sqrt(a*a+b*b)
    c2 = sqrt(c*c+d*d)
    a  = a/c1
    b  = b/c1
    c  = c/c2
    d  = d/c2
    alpha = a*c+b*d
# If they're not in the right order, bring things to
# a shuddering halt.
    if(alpha>0) {
        call intpr("error detected in qtest1",-1,1,0)
        indv(1) = i
        indv(2) = j
        indv(3) = k
        call intpr("Point being added, h:",-1,h,1)
        call intpr("now, other vertex, nxt:",-1,indv,3)
        call dblepr("Test value:",-1,alpha,1)
        call rexit("Points are collinear but h not between i and k.")
    }
# Collinear, and in the right order; think of this as meaning
# that the circumcircle in question has infinite radius.
    shdswp = .true.
}

# Get the coordinates of vertices h and j.
xh = x(h)
yh = y(h)
xj = x(j)
yj = y(j)

# Find the centre of the circumcircle of vertices h, i, k.
call circen(h,i,k,x0,y0,x,y,ntot,eps,shdswp,nerror)
if(nerror>0) return
if(shdswp) return # The points h, i, and k are colinear, so
                  # the circumcircle has `infinite radius', so
                  # (xj,yj) is definitely inside.

# Check whether (xj,yj) is inside the circle of centre
# (x0,y0) and radius r = dist[(x0,y0),(xh,yh)]

a  = x0-xh
b  = y0-yh
r2 = a*a+b*b
a  = x0-xj
b  = y0-yj
ch = a*a + b*b
if(ch<r2) shdswp = .true.
else shdswp = .false.

return
end
