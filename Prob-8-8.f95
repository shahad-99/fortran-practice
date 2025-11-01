program sum_of_array_elements
    implicit none
    integer :: NROW, NCOL 
    real,dimension(:,:), allocatable :: arr 
    integer :: iostat, alloc_stat, i, j 
    character(len=24) :: filename
    character(len=100) :: iomsg 

    ! Read filename
    write(*,1000) 
    1000 format(1x, 'This program calculate the some of each elements in an given array.',/,&
                    ' Enter the filename : ')
    read (*,'(A24)') filename
    
    ! Open Input File
    open (unit=10, file=filename, status='OLD', action='READ', iostat=iostat, iomsg=iomsg)

    if (iostat /= 0) then
        write (*,1010) iostat, iomsg
        1010 format(1x, '--- Error opening file. Iostat = ', I4,/, &
                        '--- Message : ', A) 

        stop
    else
        ! Read how-many rows and column the array have
        read (10, *) NROW, NCOL

        allocate(arr(NROW, NCOL), stat=alloc_stat)

        if (alloc_stat /= 0) stop 
        
        do i = 1, NROW 
            read (10, *) (arr(i, j), j = 1, NCOL)
        end do 
    end if 

    write (*,1020)  sum(arr)
    1020 format(1x, 'Sum of the elements = ', F10.4)

    deallocate(arr)

end program sum_of_array_elements