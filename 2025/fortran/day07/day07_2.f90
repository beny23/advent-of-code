program day07_1
    implicit none

    character(len=*), parameter :: FILENAME = "input.txt"

    character(len=1), dimension(:,:), allocatable :: mat
    integer(kind=8), dimension(:,:), allocatable :: cache
    integer :: fd, io, len, lines, i
    integer(kind=8) count

    call parse_input()

    allocate(cache(lines, len))
    cache = 0

    do i = 1, len
        if (mat(1, i) == 'S') then
            count = trace_tachyon(1, i)
            exit
        end if
    end do

    print *, "count=", count

contains

    recursive function trace_tachyon(j,i) result(count)
        implicit none
        integer, intent(in) :: i, j
        integer(kind=8) :: count

        count = cache(j, i)
        if (count == 0) then
            if (j == len) then
                count = 1
            else if (mat(j+1, i) == '^') then
                count = trace_tachyon(j+1, i-1) + trace_tachyon(j+1, i+1)
            else
                count = trace_tachyon(j+1, i)
            end if
            cache(j, i) = count
        end if
    end function trace_tachyon

    subroutine parse_input()
        implicit none

        character(len=5120) :: line
        integer :: j, i

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
            if (len == 0) then
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
    end subroutine parse_input

end program day07_1
