subroutine cross(x,y,ijk,cprd)
implicit double precision(a-h,o-z)
dimension x(3), y(3)
# Calculates a ``normalized'' cross product of the vectors joining
# [x(1),y(1)] to [x(2),y(2)] and to [x(3),y(3)] respectively.
# The normalization consists in dividing by the square of the
# shortest of the three sides of the triangle.  This normalization is
# for the purposes of testing for collinearity; if the result is less
# than epsilon, then the smallest of the sines of the angles is less than
# epsilon.

# Set constants
zero = 0.d0
one  = 1.d0
two  = 2.d0
four = 4.d0

# Adjust the coordinates depending upon which points are ideal,
# and calculate the squared length of the shortest side.

# case 0: No ideal points; no adjustment necessary.
if(ijk==0) {
	smin = -one
	do i = 1,3 {
		ip = i+1
		if(ip==4) ip = 1
		a = x(ip) - x(i)
		b = y(ip) - y(i)
		s = a*a+b*b
		if(smin < zero | s < smin) smin = s
	}
}

# case 1: Only k ideal.
if(ijk==1) {
	x(2) = x(2) - x(1)
	y(2) = y(2) - y(1)
	x(1) = zero
	y(1) = zero
	cn   = sqrt(x(2)**2+y(2)**2)
	x(2) = x(2)/cn
	y(2) = y(2)/cn
	smin  = one
}

# case 2: Only j ideal.
if(ijk==2) {
	x(3) = x(3) - x(1)
	y(3) = y(3) - y(1)
	x(1) = zero
	y(1) = zero
	cn   = sqrt(x(3)**2+y(3)**2)
	x(3) = x(3)/cn
	y(3) = y(3)/cn
	smin = one
}

# case 3: Both j and k ideal (i not).
if(ijk==3) {
		x(1) = zero
		y(1) = zero
		smin = two
}

# case 4: Only i ideal.
if(ijk==4) {
	x(3) = x(3) - x(2)
	y(3) = y(3) - y(2)
	x(2) = zero
	y(2) = zero
	cn   = sqrt(x(3)**2+y(3)**2)
	x(3) = x(3)/cn
	y(3) = y(3)/cn
	smin = one
}

# case 5: Both i and k ideal (j not).
if(ijk==5) {
	x(2) = zero
	y(2) = zero
	smin = two
}

# case 6: Both i and j ideal (k not).
if(ijk==6) {
	x(3) = zero
	y(3) = zero
	smin = two
}

# case 7: All three points ideal; no adjustment necessary.
if(ijk==7) {
	smin = four
}

a = x(2)-x(1)
b = y(2)-y(1)
c = x(3)-x(1)
d = y(3)-y(1)

cprd = (a*d - b*c)/smin
return
end
