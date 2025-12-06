program day05_2
    implicit none

    character(len=*), parameter :: FILENAME = "input.txt"

    character(len=160) :: line
    integer(kind=8), dimension(:), allocatable :: s, e
    integer(kind=8) :: total
    integer :: fd, io, num_rules, pos, len, i, j

    ! Open the file for reading
    open(newunit=fd, file=FILENAME, status="old")

    num_rules = 0

    ! Read the file line by line
    ! idiomatic fortran, process the file twice
    do
        read(fd, "(A)", iostat=io) line

        ! Exit if end of file is reached
        if (io /= 0) exit

        if (index(line, '-') > 0) then
            num_rules = num_rules + 1
        end if
    end do

    print *, "num_rules=", num_rules

    ! Close the file
    close(fd)

    ! open again to read in the data, now we have the data structure sizes
    allocate(s(num_rules))
    allocate(e(num_rules))

    ! Open the file for reading
    open(newunit=fd, file=FILENAME, status="old")

    do i = 1, num_rules
        read(fd, "(A)") line

        pos = index(line, '-')
        len = len_trim(line)

        read(line(1:(pos-1)), *) s(i)
        read(line((pos+1):len), *) e(i)
    end do

    ! Close the file
    close(fd)

    do j = 2, num_rules
        do i = 1, j-1
            if (s(i) == 0) cycle
            ! j completely inside i
            if (s(i) <= s(j) .and. e(i) >= e(j)) then
                s(j) = 0
                exit
            ! i and j overlap one end
            else if (s(i) < s(j) .and. e(i) >= s(j) .and. e(i) < e(j)) then
                s(j) = e(i) + 1
            ! i and j overlap the other end
            else if (s(j) < s(i) .and. e(j) >= s(i) .and. e(j) < e(i)) then
                e(j) = s(i) - 1
            ! i completely inside j
            else if (s(j) <= s(i) .and. e(j) >= e(i)) then
                s(i) = 0
            end if
        end do
    end do

    total = 0
    do i = 1, num_rules
        if (s(i) == 0) cycle
        total = total + e(i) - s(i) + 1
    end do

    print *, "total=", total

end program day05_2
