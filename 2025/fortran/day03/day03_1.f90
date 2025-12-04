program day03_1
    implicit none
    character(len=128) :: line
    integer :: fd, io, high_volt, low_volt, high_pos, sum, len, i, volt

    ! Open the file for reading
    open(newunit=fd, file="input.txt", status="old")

    sum = 0

    ! Read the file line by line
    do
        read(fd, "(A)", iostat=io) line

        ! Exit if end of file is reached
        if (io /= 0) exit

        print *, line

        high_volt = 0
        low_volt = 0

        len = len_trim(line)

        do i = 1, len-1
            read(line(i:i), *) volt
            if (volt > high_volt) then
                high_volt = volt
                high_pos = i
            end if
        end do

        do i = high_pos+1, len
            read(line(i:i), *) volt
            if (volt > low_volt) then
                low_volt = volt
            end if
        end do

        volt = high_volt * 10 + low_volt
        print *, "volt=", volt
        sum = sum + volt

    end do

    print *, "sum=", sum

    ! Close the file
    close(fd)

end program day03_1
