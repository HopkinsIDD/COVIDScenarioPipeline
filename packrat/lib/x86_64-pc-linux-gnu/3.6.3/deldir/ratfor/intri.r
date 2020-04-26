subroutine intri(x,y,u,v,n,okay)
#
# Test whether any of the points (u(i),v(i)) are inside the triangle
# whose vertices are specified by the vectors x and y.
# Called by .Fortran() from triang.list.R.
#

implicit double precision(a-h,o-z)
dimension x(3), y(3), u(n), v(n)
integer okay
logical inside

zero = 0.d0

# Check on order (clockwise or anticlockwise).
s = 1.d0
a = x(2) - x(1)
b = y(2) - y(1)
c = x(3) - x(1)
d = y(3) - y(1)
cp = a*d - b*c
if(cp < 0) s = -s
do i = 1,n {
	inside = .true.
	do j = 1,3 {
        	jp = j+1
        	if(jp==4) jp = 1 # Take addition modulo 3.
		a  = x(jp) - x(j)
		b  = y(jp) - y(j)
		c  = u(i)  - x(j)
		d  = v(i)  - y(j)
		cp = s*(a*d - b*c)
		if(cp <= zero) {
			inside = .false.
			break
		}
	}
	if(inside) {
		okay = 0
		return
	}
	
}
okay = 1
return
end
