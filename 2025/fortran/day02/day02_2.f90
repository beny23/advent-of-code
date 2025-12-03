program day02_2
    implicit none
    character(len=500) :: line
    integer :: fd, len, pos, start, end, dash, d, j, k
    integer(kind=8) :: from, to, i, sum, num, mod_f, temp

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

        ! gets the range
        print *, from, ' - ', to

        per_i: do i = from, to

            d = num_digits(i)

            ! brute force every possible length of numbers
            per_j: do j = 1, d/2

                ! if the partials fit neatly into the number
                if (mod(d, j) == 0) then

                    ! get the first partial (lowest significant)
                    mod_f = 10 ** j
                    num = mod(i, mod_f)
                    temp = i / mod_f

                    ! try every other combination of partials of length j
                    do k = 2, d/j
                        if (num /= mod(temp, mod_f)) then
                            cycle per_j
                        end if
                        temp = temp / mod_f
                    end do

                    print *, "found one:", i
                    sum = sum + i
                    cycle per_i
                end if
            end do per_j

        end do per_i

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

end program day02_2
