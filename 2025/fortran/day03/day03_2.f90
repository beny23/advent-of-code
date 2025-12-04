program day03_2
    implicit none
    character(len=128) :: line
    integer :: fd, io, digit_no, last_i, i, volt, high_volt, high_pos, len
    integer(kind=8) :: sum, jolt

    ! Open the file for reading
    open(newunit=fd, file="input.txt", status="old")

    sum = 0

    ! Read the file line by line
    do
        read(fd, "(A)", iostat=io) line

        ! Exit if end of file is reached
        if (io /= 0) exit

        print *, line

        len = len_trim(line)
        jolt = 0
        last_i = 0

        do digit_no = 11, 0, -1
            high_volt = 0
            ! go backwards, leaving enough space to find the biggest number
            do i = len-digit_no, last_i+1, -1
                read(line(i:i), *) volt
                if (volt >= high_volt) then
                    high_volt = volt
                    high_pos = i
                end if
            end do

            jolt = jolt * 10 + high_volt
            last_i = high_pos
        end do

        print *, "jolt=", jolt
        sum = sum + jolt

    end do

    print *, "sum=", sum

    ! Close the file
    close(fd)

end program day03_2
