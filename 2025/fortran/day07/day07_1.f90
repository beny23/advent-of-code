program day07_1
    implicit none

    character(len=*), parameter :: FILENAME = "input.txt"

    character(len=1), dimension(:,:), allocatable :: mat
    character(len=5120) :: line
    integer :: fd, io, len, lines, splits, i, j

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

    splits = 0

    do j = 1, lines
        print *, mat(j, :)
    end do

    do j = 1, lines
        do i = 1, lines-1

            if (j > 1 .and. (mat(j-1, i) == '|' .or. mat(j-1, i) == 'S') .and. mat(j, i) == '.') then
                mat(j, i) = '|'
            else if (mat(j, i) == '^' .and. mat(j-1, i) == '|') then
                mat(j, i-1) = '|'
                mat(j, i+1) = '|'
                splits = splits + 1
            end if
        end do
    end do

    do j = 1, lines
        print *, mat(j, :)
    end do

    print *, "splits=", splits
end program day07_1
