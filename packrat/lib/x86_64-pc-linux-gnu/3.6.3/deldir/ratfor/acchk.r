subroutine acchk(i,j,k,anticl,x,y,ntot,eps)
# Check whether vertices i, j, k, are in anti-clockwise order.
# Called by locn, qtest, qtest1.

implicit double precision(a-h,o-z)
dimension x(-3:ntot), y(-3:ntot), xt(3), yt(3)
logical anticl

# Create indicator telling which of i, j, and k are ideal points.
if(i<=0) i1 = 1
else i1 = 0
if(j<=0) j1 = 1
else j1 = 0
if(k<=0) k1 = 1
else k1 = 0
ijk = i1*4+j1*2+k1

# Get the coordinates of vertices i, j, and k. (Pseudo-coordinates for
# any ideal points.)
xt(1) = x(i)
yt(1) = y(i)
xt(2) = x(j)
yt(2) = y(j)
xt(3) = x(k)
yt(3) = y(k)

# Get the ``normalized'' cross product.
call cross(xt,yt,ijk,cprd)

# If cprd is positive then (ij-cross-ik) is directed ***upwards*** 
# and so i, j, k, are in anti-clockwise order; else not.
if(cprd > eps) anticl = .true.
else anticl = .false.
return
end
