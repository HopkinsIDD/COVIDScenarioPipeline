subroutine dirseg(dirsgs,ndir,nadj,madj,npd,x,y,ntot,rw,eps,ntri,nerror)

# Output the endpoints of the segments of boundaries of Dirichlet
# tiles.  (Do it economically; each such segment once and only once.)
# Called by master.

implicit double precision(a-h,o-z)
logical collin, adjace, intfnd, bptab, bptcd, goferit, rwu
dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot)
dimension dirsgs(10,ndir), rw(4)

nerror = -1

# Add in some dummy corner points, outside the actual window.
# Far enough out so that no resulting tile boundaries intersect the
# window.

# Note that these dummy corners are needed by the routine `dirout'
# but will screw things up for `delseg' and `delout'.  Therefore
# this routine (`dirseg') must be called ***before*** dirout, and
# ***after*** delseg and delout.

# Dig out the corners of the rectangular window.
xmin = rw(1)
xmax = rw(2)
ymin = rw(3)
ymax = rw(4)

a = xmax-xmin
b = ymax-ymin
c = sqrt(a*a+b*b)

npd  = ntot-4
nstt = npd+1
i = nstt
x(i) = xmin-c
y(i) = ymin-c
i = i+1
x(i) = xmax+c
y(i) = ymin-c
i = i+1
x(i) = xmax+c
y(i) = ymax+c
i = i+1
x(i) = xmin-c
y(i) = ymax+c

do j = nstt,ntot {
	call addpt(j,nadj,madj,x,y,ntot,eps,ntri,nerror)
        ntri = ntri + 3
	if(nerror > 0) {
            return
        }
}

# Put the segments into the array dirsgs.

# For each distinct pair of (genuine) data points, find out if they are
# adjacent.  If so, find the circumcentres of the triangles lying on each
# side of the segment joining them.
kseg = 0
do i = 2,npd {
        do j = 1,i-1 {
                call adjchk(i,j,adjace,nadj,madj,ntot,nerror)
		if(nerror > 0) {
                    return
                }
                if(adjace) {
                        call pred(k,i,j,nadj,madj,ntot,nerror)
			if(nerror > 0) return
                        call circen(i,k,j,a,b,x,y,ntot,eps,collin,nerror)
			if(nerror > 0) return
                        if(collin) {
				nerror = 12
				return
			}
                        call succ(l,i,j,nadj,madj,ntot,nerror)
			if(nerror > 0) return
                        call circen(i,j,l,c,d,x,y,ntot,eps,collin,nerror)
			if(nerror > 0) return
                        if(collin) {
				nerror = 12
				return
			}
                        # If a circumcentre is outside the rectangular window
                        # of interest, draw a line joining it to the other
                        # circumcentre.  Find the intersection of this line with
                        # the boundary of the window; for (a,b) and call the point
                        # of intersection (ai,bi).  For (c,d), call it (ci,di).
                        # Note: rwu = "right way up".
                        xi = x(i)
                        xj = x(j)
                        yi = y(i)
                        yj = y(j)
                        if(yi!=yj) {
                            slope = (xi - xj)/(yj - yi)
                            rwu   = .true.
                        } else {
                            slope = 0.d0
                            rwu = .false.
                        }
                        call dldins(a,b,slope,rwu,ai,bi,rw,intfnd,bptab,nedgeab)
			if(!intfnd) {
				nerror = 16
				return
			}
                        call dldins(c,d,slope,rwu,ci,di,rw,intfnd,bptcd,nedgecd)
			if(!intfnd) {
				nerror = 16
				return
			}
			goferit = .false.
			if(bptab & bptcd) {
				xm = 0.5*(ai+ci)
				ym = 0.5*(bi+di)
				if(xmin<xm&xm<xmax&ymin<ym&ym<ymax) {
					goferit = .true.
				}
			}
			if((!bptab)|(!bptcd)) goferit = .true.
			if(goferit) {
				kseg = kseg + 1
				if(kseg > ndir) {
					nerror = 15
					return
				}
				dirsgs(1,kseg) = ai
				dirsgs(2,kseg) = bi
				dirsgs(3,kseg) = ci
				dirsgs(4,kseg) = di
				dirsgs(5,kseg) = i
				dirsgs(6,kseg) = j
				if(bptab) dirsgs(7,kseg) = 1.d0
				else dirsgs(7,kseg) = 0.d0
				if(bptcd) dirsgs(8,kseg) = 1.d0
				else dirsgs(8,kseg) = 0.d0
                                if(bptab) dirsgs(9,kseg) = -nedgeab
                                else dirsgs(9,kseg) = k
                                if(bptcd) dirsgs(10,kseg) = -nedgecd
                                else dirsgs(10,kseg) = l
			}
                }
        }
}
ndir = kseg

return
end
