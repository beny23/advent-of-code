program day06_2
    implicit none

    character(len=*), parameter :: FILENAME = "input.txt"

    character(len=1), dimension(:,:), allocatable :: mat
    character(len=1) :: op
    character(len=5120) :: line
    integer(kind=8) result, total
    integer :: fd, io, len, i, j, lines, val

    lines = 0
    len = 0

    ! Open the file for reading
    open(newunit=fd, file=FILENAME, status="old")

    ! Read the file line by line, only interested in the last line
    ! idiomatic fortran, process the file twice
    do
        read(fd, "(A)", iostat=io) line

        ! Exit if end of file is reached
        if (io /= 0) exit

        lines = lines + 1
        if (len < len_trim(line)) then
            len = len_trim(line)
        end if
    end do

    ! Close the file
    close(fd)

    print *, "len=", len, "lines=", lines

    allocate(mat(lines, len))

    ! Open the file for reading
    open(newunit=fd, file=FILENAME, status="old")

    do j = 1, lines
        read(fd, "(A)", iostat=io) line

        ! Exit if end of file is reached
        if (io /= 0) exit

        do i = 1, len
            mat(j, i) = line(i:i)
        end do
    end do

    ! Close the file
    close(fd)

    result = 0
    total = 0

    do i = 1, len
        if (mat(lines, i) /= ' ') then
            total = total + result
            op = mat(lines, i)
            result = 0
        end if

        line = ''
        do j = 1, lines-1
            line(j:j) = mat(j, i)
        end do

        read(line, *, iostat=io) val

        if (io /= 0) cycle

        if (result == 0) then
            result = val
        else if (op == '*') then
            result = result * val
        else
            result = result + val
        end if

    end do

    total = total + result

    print *, "total=", total
end program day06_2
