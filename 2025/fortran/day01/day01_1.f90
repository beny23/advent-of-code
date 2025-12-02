program day01_1
    implicit none
    character :: dir
    integer :: clicks, io, fd, pos, zeros

    ! Open the file for reading
    open(newunit=fd, file="input.txt", status="old")

    pos = 50
    zeros = 0

    ! Read the file line by line
    do
        read(fd, "(a,i16)", iostat=io) dir, clicks

        ! Exit if end of file is reached
        if (io /= 0) exit

        if (dir == "L") then
            clicks = -clicks
        end if

        pos = modulo(pos + clicks, 100)

        if (pos == 0) then
            zeros = zeros + 1
        end if
    end do

    print *, zeros

    ! Close the file
    close(fd)

end program day01_1
