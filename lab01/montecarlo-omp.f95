program montecarlo
	use omp_lib

	real*8 :: almostPi, x, y, ran0
	integer :: c = 0
	integer :: i, samples, tid
	integer :: threads = 4
	real t1, t2

	print*, "How often: "
	read*, samples

	call cpu_time(t1)
	!$omp parallel private(hits,tid,i,r,x,y) num_threads(threads)
	tid = omp_get_thread_num()

	do i = 1, samples/threads
			x = ran0(tid)
			y = ran0(tid)

			if (x*x + y*y <= 1) then
				!$omp critical
					c = c + 1
				!$omp end critical
			end if
	end do
	!$omp end parallel
	call cpu_time(t2)

	almostPi = real(c) / real(samples) * 4.0

	WRITE(*, '(F9.6)') almostPi
	WRITE(*, '(F9.6)') t2-t1
end

function  ran0(seed)
! Park & Miller Random Generator
integer*4 :: seed,ia,im,iq,ir,mask,k
real*8 :: ran0,am
parameter  (ia=16807,im=2147483647,am=1./im,iq=127773,ir=2836,mask=123459876)
seed=ieor(seed,mask)
k=seed/iq
seed=ia*(seed-k*iq)-ir*k
if  (seed.lt.0)  seed=seed+im
ran0=am*seed
seed=ieor(seed,mask)
return
end