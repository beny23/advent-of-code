program day08_1
    implicit none

    type :: pair_t
        integer :: i, j
        real :: distance
    end type pair_t

    character(len=*), parameter :: FILENAME = "input.txt"
    integer, parameter :: FIRST_N = 1000

    real, dimension(:, :), allocatable :: values
    integer, dimension(:), allocatable :: groups
    integer, dimension(:), allocatable :: group_sizes
    type(pair_t), dimension(:), allocatable :: pairs
    type(pair_t) :: temp
    character(len=128) :: line
    integer :: fd, io, lines, i, j, g, g2, group_counter, total, max, pair, number_pairs

    lines = 0

    ! Open the file for reading
    ! reading the file twice is idiomatic fortran
    open(newunit=fd, file=FILENAME, status="old")

    ! Read the file line by line
    do
        read(fd, "(A)", iostat=io) line

        ! Exit if end of file is reached
        if (io /= 0) exit

        lines = lines + 1
    end do

    ! Close the file
    close(fd)

    allocate(values(3, lines))

    ! the groups is an array that holds which circuit every node belongs to
    allocate(groups(lines))

    ! this array will hold the sizes of the groups
    allocate(group_sizes(lines)) !won't need that many, but good enough
    number_pairs = (lines * (lines - 1)) / 2
    allocate(pairs(number_pairs))

    groups = 0
    group_sizes = 0

    ! read the values
    open(newunit=fd, file=FILENAME, status="old")
    do i = 1, lines
        read(fd, *) values(:, i)
    end do
    close(fd)

    ! calculate the distances
    pair = 1
    do i = 1, lines - 1
        do j = i + 1, lines
            pairs(pair)%i = i
            pairs(pair)%j = j
            pairs(pair)%distance = sqrt(sum((values(:, i) - values(:, j)) ** 2))
            pair = pair + 1
        end do
    end do

    ! sort the pairs, so we've got them in ascending distance
    call quicksort(1, number_pairs)

    group_counter = 0

    ! go through the first N pairs and calculate the groups
    do i = 1, FIRST_N
        temp = pairs(i)

        ! if neither are in a group already
        if (groups(temp%i) == 0 .and. groups(temp%j) == 0) then
            group_counter = group_counter + 1
            groups(temp%i) = group_counter
            groups(temp%j) = group_counter
            group_sizes(group_counter) = 2
        ! if one of them is in a group
        else if (groups(temp%i) == 0) then
            g = groups(temp%j)
            groups(temp%i) = g
            group_sizes(g) = group_sizes(g) + 1
        ! if the other is in a group
        else if (groups(temp%j) == 0) then
            g = groups(temp%i)
            groups(temp%j) = g
            group_sizes(g) = group_sizes(g) + 1
        ! if they are in different groups
        else if (groups(temp%i) /= groups(temp%j)) then
            g = groups(temp%i)
            g2 = groups(temp%j)
            if (group_sizes(g) < group_sizes(g2)) then
                j = g
                g = g2
                g2 = j
            end if
            group_sizes(g) = group_sizes(g) + group_sizes(g2)
            group_sizes(g2) = 0

            do j = 1, lines
                if (groups(j) == g2) then
                    groups(j) = g
                end if
            end do
        end if
    end do

    total = 1
    do j = 1, 3
        max = 0
        g = 0
        do i = 1, group_counter
            if (group_sizes(i) > max) then
                max = group_sizes(i)
                g = i
            end if
        end do
        print *, "g=", g, "size=", group_sizes(g)
        group_sizes(g) = 0
        total = total * max
    end do

    print *, "total=", total

contains

    recursive subroutine quicksort(lo, hi)
        implicit none
        integer, intent(in) :: lo, hi
        integer :: i, j
        real :: pivot
        type(pair_t) :: temp

        if (lo >= hi) return

        pivot = pairs((lo + hi) / 2)%distance
        i = lo
        j = hi

        do
            do while (pairs(i)%distance < pivot)
                i = i + 1
            end do

            do while (pairs(j)%distance > pivot)
                j = j - 1
            end do

            if (i >= j) exit

            temp = pairs(i)
            pairs(i) = pairs(j)
            pairs(j) = temp

            i = i + 1
            j = j - 1
        end do

        call quicksort(lo, j)
        call quicksort(j + 1, hi)

    end subroutine quicksort

end program day08_1
