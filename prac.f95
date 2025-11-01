program read_from_file
    implicit none

    character(len=24) :: filename 
    integer :: a, b, sum
 

    write (*,*) 'Write input file name : '
    read (*,*) filename 

    open(unit=10, file=filename)
    read (10, *) a, b

    sum = a + b 
    write (*,*) 'Sum of a + b = ', sum 
end program read_from_file