program day05_1
    implicit none

    type :: range_t
        integer(kind=8) :: start, end
    end type range_t

    character(len=*), parameter :: FILENAME = "input.txt"

    character(len=160) :: line
    type(range_t), dimension(:), allocatable :: rules
    integer(kind=8), dimension(:), allocatable :: ids
    integer(kind=8) :: id
    integer :: fd, io, num_rules, num_ids, pos, len, i, j, total

    ! Open the file for reading
    open(newunit=fd, file=FILENAME, status="old")

    num_rules = 0
    num_ids = 0

    ! Read the file line by line
    ! idiomatic fortran, process the file twice
    do
        read(fd, "(A)", iostat=io) line

        ! Exit if end of file is reached
        if (io /= 0) exit

        if (len_trim(line) > 0) then
            if (index(line, '-') > 0) then
                num_rules = num_rules + 1
            else
                num_ids = num_ids + 1
            end if
        end if
    end do

    print *, "num_rules=", num_rules, "num_ids=", num_ids

    ! Close the file
    close(fd)

    ! open again to read in the data, now we have the data structure sizes
    allocate(rules(num_rules))
    allocate(ids(num_ids))

    ! Open the file for reading
    open(newunit=fd, file=FILENAME, status="old")

    do i = 1, num_rules
        read(fd, "(A)") line

        pos = index(line, '-')
        len = len_trim(line)

        read(line(1:(pos-1)), *) rules(i)%start
        read(line((pos+1):len), *) rules(i)%end
    end do

    ! read the blank line
    read(fd, "(A)")

    do i = 1, num_ids
        read(fd, *) ids(i)
    end do

    ! Close the file
    close(fd)

    total = 0
    do i = 1, num_ids
        id = ids(i)

        do j = 1, num_rules
            if (id >= rules(j)%start .and. id <= rules(j)%end) then
                total = total + 1
                exit
            end if
        end do
    end do

    print *, "total=", total

end program day05_1
