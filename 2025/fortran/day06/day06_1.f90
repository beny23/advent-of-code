program day06_1
    implicit none

    character(len=*), parameter :: FILENAME = "input.txt"

    character(len=5120) :: line
    character(len=5120) :: ops
    integer(kind=8), dimension(:), allocatable :: result
    integer(kind=8), dimension(:), allocatable :: vals
    integer(kind=8) :: total
    integer :: fd, io, io2, len, i, j, lines

    ! Open the file for reading
    open(newunit=fd, file=FILENAME, status="old")

    lines = 0

    ! Read the file line by line, only interested in the last line
    ! idiomatic fortran, process the file twice
    do
        read(fd, "(A)", iostat=io) line

        ! Exit if end of file is reached
        if (io /= 0) exit

        lines = lines + 1
    end do

    lines = lines - 1

    ! Close the file
    close(fd)

    len = 0

    do i = 1, len_trim(line)

        if (line(i:i) /= ' ') then
            len = len + 1
            ops(len:len) = line(i:i)
        end if

    end do

    print *, "lines=", lines, "len=", len, "ops=", ops(1:len)

    allocate(result(len))
    allocate(vals(len))

    ! Open the file for reading
    open(newunit=fd, file=FILENAME, status="old")

    ! Read the file line by line again
    ! idiomatic fortran, process the file twice
    do j = 1, lines
        read(fd, *) vals

        do i = 1, len
            print *, "j=", j, "i=", i, "val=", vals(i)

            if (j == 1) then
                result(i) = vals(i)
            else if (ops(i:i) == '+') then
                result(i) = result(i) + vals(i)
            else
                result(i) = result(i) * vals(i)
            end if
        end do
    end do

    total = 0
    do i = 1, len
        print *, "i=", i, "result=", result(i)
        total = total + result(i)
    end do

    print *, "total=", total

    ! Close the file
    close(fd)

end program day06_1
