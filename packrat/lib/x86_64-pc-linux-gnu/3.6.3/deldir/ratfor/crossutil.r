subroutine crossutil(i,j,k,x,y,ntot,eps,collin)
implicit double precision(a-h,o-z)
dimension x(-3:ntot), y(-3:ntot)
dimension xt(3), yt(3)
logical collin

xt(1) = x(i)
yt(1) = y(i)
xt(2) = x(j)
yt(2) = y(j)
xt(3) = x(k)
yt(3) = y(k)

# Create indicator telling which of i, j, and k are ideal points.
# The point being added, i, is never ideal.
i1 = 0
if(j<=0) j1 = 1
else j1 = 0
if(k<=0) k1 = 1
else k1 = 0
ijk = i1*4+j1*2+k1

call cross(xt,yt,ijk,cprd)
collin = (abs(cprd) < eps)
return
end
