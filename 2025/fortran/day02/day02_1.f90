program day02_1
    implicit none
    character(len=500) :: line
    integer :: fd, io, len, pos, start, end, dash, d
    integer(kind=8) :: from, to, i, factor, i_high, i_low, sum

    ! Open the file for reading
    open(newunit=fd, file="input.txt", status="old", action="read")

    read(fd, "(A)") line
    ! Close the file
    close(fd)

    len = len_trim(line)

    start = 1
    sum = 0

    do
        pos = index(line(start:len), ',')
        if (pos == 0) then
            end = len
        else
            end = pos - 1 + start - 1
        end if

        dash = index(line(start:end), '-') + start - 1

        read(line(start:(dash-1)), *) from
        read(line((dash+1):end), *) to

        print *, from, ' - ', to

        i = from
        do while (i <= to)

            d = num_digits(i)

            ! if doesn't have an even number of digits, it can't be right
            ! so go to the next number that does have the right number of digits
            if (mod(d, 2) /= 0) then
                i = 10 ** d
                cycle
            end if

            factor = 10 ** (d / 2)

            ! slight in to a higher half and a lower half,
            ! e.g. 1235 -> 12 and 35
            i_high = i / factor
            i_low = mod(i, factor)

            ! if the higher half is higher, then skips steps to the higher one
            if (i_high > i_low) then
                i = i_high * factor + i_high
                cycle
            end if

            ! if the higher half is lower, then skip to the next higher one
            if (i_high < i_low) then
                i = (i_high + 1) * factor
                cycle
            end if

            print *, "found one:", i
            sum = sum + i

            i = i + 1
        end do

        if (pos == 0) exit
        start = end + 2
    end do

    print *, "Sum:", sum

contains

    function num_digits(n) result(digits)
        implicit none
        integer(kind=8), intent(in) :: n
        integer :: digits
        integer(kind=8) :: temp

        temp = n
        digits = 0
        do
            digits = digits + 1
            temp = temp / 10
            if (temp == 0) exit
        end do
    end function num_digits

end program day02_1
