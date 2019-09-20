program montecarlo
	use omp_lib

	real*8 :: almostPi, x, y, ran0
	integer :: c = 0
	integer :: i, samples, tid
	real t1, t2

	print*, "How often: "
	read*, samples

	call cpu_time(t1)

	do i = 1, samples
			x = rand(0)
			y = rand(0)

			if (x*x + y*y <= 1) then
				c = c + 1
			end if
	end do
	call cpu_time(t2)

	almostPi = real(c) / real(samples) * 4.0

	WRITE(*, '(F9.6)') almostPi
	WRITE(*, '(F9.6)') t2-t1
end
