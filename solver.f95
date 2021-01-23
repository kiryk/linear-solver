program main
  implicit none

  integer :: height, width, i, j
  real, allocatable, dimension(:,:) :: matrix

  print *, 'number of equations:'
  read *, height

  width = height + 1

  allocate(matrix(height, width))

  do i = 1, height
    do j = 1, width
      read *, matrix(i, j)
    end do
  end do

  call solve(matrix, height, width)

  do i = 1, height
    print *, matrix(i, :)
  end do
end program main

subroutine solve(matrix, height, width)
  implicit none

  integer :: height, width, n, i
  real, dimension(height, width) :: matrix
  real, dimension(width) :: row

  do n = 1, height
    i = n
    do while (i <= height .and. matrix(i, n) == 0)
      i = i + 1
    end do

    if (i <= height) then
      row = matrix(i, :)
      matrix(i, :) = matrix(n, :)
      matrix(n, :) = row

      matrix(n, :) = matrix(n, :)/matrix(n, n)

      do i = 1, height
        if (i /= n .and. matrix(i, n) /= 0) then
          matrix(i, :) = matrix(i, :) - matrix(n, :)*matrix(i, n)
        end if
      end do
    end if
  end do
end subroutine solve
