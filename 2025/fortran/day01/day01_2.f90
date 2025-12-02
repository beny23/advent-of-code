program day01_2
    implicit none
    character :: dir
    integer :: clicks, io, fd, pos, new_pos, zeros, new_zeros

    ! Open the file for reading
    open(newunit=fd, file="input.txt", status="old")

    pos = 50
    zeros = 0

    ! Read the file line by line
    do
        read(fd, "(a,i16)", iostat=io) dir, clicks

        ! Exit if end of file is reached
        if (io /= 0) exit

        print *, pos, ",", zeros, ",", dir, ",", clicks
        if (dir == "L") then
            new_pos = pos - clicks
            if (new_pos <= 0) then
                zeros = zeros + ((-new_pos) / 100)
                if (pos /= 0) then
                    zeros = zeros + 1
                end if
            end if
        else
            new_pos = pos + clicks
            zeros = zeros + (new_pos / 100)
        end if

        pos = modulo(new_pos, 100)

    end do

    print *, zeros

    ! Close the file
    close(fd)

end program day01_2
