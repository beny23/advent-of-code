program day04_1
    implicit none
    character(len=160) :: line
    character(len=:), allocatable :: buffer
    integer :: fd, io, width, height, total, x, y

    ! Open the file for reading
    open(newunit=fd, file="input.txt", status="old")

    width = 0
    height = 0

    ! Read the file line by line
    do
        read(fd, "(A)", iostat=io) line

        ! Exit if end of file is reached
        if (io /= 0) exit

        ! Add a blank row at the beginning
        if (width == 0) then
            width = len_trim(line)
            buffer = repeat('.', width + 2)
        end if

        buffer = buffer // '.' // trim(line) // '.'
        height = height + 1
    end do

    ! add a blank row at the end
    buffer = buffer // repeat('.', width + 2)

    print *, "buffer=", buffer

    total = 0
    do y = 1, height
        do x = 1, width
            if (is_paper(x, y) > 0 .and. can_be_accessed(x, y)) then
                total = total + 1
            end if
        end do
    end do

    print *, "total=", total

    ! Close the file
    close(fd)

contains

    function is_paper(x, y) result(paper)
        integer, intent(in) :: x, y
        integer :: pos
        integer :: paper

        ! 1-based positions
        pos = x + y * (width + 2) + 1

        if (buffer(pos:pos) /= '.') then
            paper = 1
        else
            paper = 0
        end if
    end function is_paper

    function can_be_accessed(x, y) result(accessible)
        integer, intent(in) :: x, y
        integer :: papers
        logical :: accessible

        papers = is_paper(x - 1, y - 1) + &
                 is_paper(x, y - 1) + &
                 is_paper(x + 1, y - 1) + &
                 is_paper(x - 1, y) + &
                 is_paper(x + 1, y) + &
                 is_paper(x - 1, y + 1) + &
                 is_paper(x, y + 1) + &
                 is_paper(x + 1, y + 1)

        accessible = papers < 4
    end function can_be_accessed

end program day04_1
