subroutine triar(x0,y0,x1,y1,x2,y2,area)

# Calculate the area of a triangle with given
# vertices, in anti clockwise direction.
# Called by delout.

implicit double precision(a-h,o-z)
half = 0.5d0

area = half*((x1-x0)*(y2-y0)-(x2-x0)*(y1-y0))
return
end
