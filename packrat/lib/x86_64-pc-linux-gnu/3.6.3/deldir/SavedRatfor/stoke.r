subroutine stoke(x1,y1,x2,y2,rw,area,s1,eps,nerror)

# Apply Stokes' theorem to find the area of a polygon;
# we are looking at the boundary segment from (x1,y1)
# to (x2,y2), travelling anti-clockwise.  We find the
# area between this segment and the horizontal base-line
# y = ymin, and attach a sign s1.  (Positive if the
# segment is right-to-left, negative if left to right.)
# The area of the polygon is found by summing the result
# over all boundary segments.

# Just in case you thought this wasn't complicated enough,
# what we really want is the area of the intersection of
# the polygon with the rectangular window that we're using.

# Called by dirout.

implicit double precision(a-h,o-z)
dimension rw(4)
logical value

zero   = 0.d0
nerror = -1

# If the segment is vertical, the area is zero.
call testeq(x1,x2,eps,value)
if(value) {
        area = 0.
        s1   = 0.
        return
}

# Find which is the right-hand end, and which is the left.
if(x1<x2) {
        xl = x1
        yl = y1
        xr = x2
        yr = y2
        s1 = -1.
}
else {
        xl = x2
        yl = y2
        xr = x1
        yr = y1
        s1 = 1.
}

# Dig out the corners of the rectangular window.
xmin = rw(1)
xmax = rw(2)
ymin = rw(3)
ymax = rw(4)

# Now start intersecting with the rectangular window.
# Truncate the segment in the horizontal direction at
# the edges of the rectangle.
slope = (yl-yr)/(xl-xr)
x  = max(xl,xmin)
y  = yl+slope*(x-xl)
xl = x
yl = y

x  = min(xr,xmax)
y  = yr+slope*(x-xr)
xr = x
yr = y

if(xr<=xmin|xl>=xmax) {
        area = 0.
        return
}

# We're now looking at a trapezoidal region which may or may
# not protrude above or below the horizontal strip bounded by
# y = ymax and y = ymin.
ybot = min(yl,yr)
ytop = max(yl,yr)

# Case 1; ymax <= ybot:
# The `roof' of the trapezoid is entirely above the
# horizontal strip.
if(ymax<=ybot) {
        area = (xr-xl)*(ymax-ymin)
        return
}

# Case 2; ymin <= ybot <= ymax <= ytop:
# The `roof' of the trapezoid intersects the top of the
# horizontal strip (y = ymax) but not the bottom (y = ymin).
if(ymin<=ybot&ymax<=ytop) {
        call testeq(slope,zero,eps,value)
        if(value) {
                w1 = 0.
                w2 = xr-xl
        }
        else {
                xit = xl+(ymax-yl)/slope
                w1 = xit-xl
                w2 = xr-xit
                if(slope<0.) {
                        tmp = w1
                        w1  = w2
                        w2  = tmp
                }
        }
        area = 0.5*w1*((ybot-ymin)+(ymax-ymin))+w2*(ymax-ymin)
        return
}

# Case 3; ybot <= ymin <= ymax <= ytop:
# The `roof' intersects both the top (y = ymax) and
# the bottom (y = ymin) of the horizontal strip.
if(ybot<=ymin&ymax<=ytop) {
        xit = xl+(ymax-yl)/slope
        xib = xl+(ymin-yl)/slope
        if(slope>0.) {
                w1 = xit-xib
                w2 = xr-xit
        }
        else {
                w1 = xib-xit
                w2 = xit-xl
        }
        area = 0.5*w1*(ymax-ymin)+w2*(ymax-ymin)
        return
}

# Case 4; ymin <= ybot <= ytop <= ymax:
# The `roof' is ***between*** the bottom (y = ymin) and
# the top (y = ymax) of the horizontal strip.
if(ymin<=ybot&ytop<=ymax) {
        area = 0.5*(xr-xl)*((ytop-ymin)+(ybot-ymin))
        return
}

# Case 5; ybot <= ymin <= ytop <= ymax:
# The `roof' intersects the bottom (y = ymin) but not
# the top (y = ymax) of the horizontal strip.
if(ybot<=ymin&ymin<=ytop) {
        call testeq(slope,zero,eps,value)
        if(value) {
                area = 0.
                return
        }
        xib = xl+(ymin-yl)/slope
        if(slope>0.) w = xr-xib
        else w = xib-xl
        area = 0.5*w*(ytop-ymin)
        return
}

# Case 6; ytop <= ymin:
# The `roof' is entirely below the bottom (y = ymin), so
# there is no area contribution at all.
if(ytop<=ymin) {
        area = 0.
        return
}

# Default; all stuffed up:
nerror = 8
return

end
