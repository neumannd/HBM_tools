module process_layer_data

  implicit none
  
contains

  subroutine sum_layer_data(vol_4d, vals_4d, vals_3d, vals_1d, size_4d)
  
    real(8), dimension(:,:,:,:),              intent(in)  :: vol_4d, vals_4d
    real(8), dimension(:,:,:),   allocatable, intent(OUT) :: vals_3d
    real(8), dimension(:),       allocatable, intent(OUT) :: vals_1d
    integer, dimension(4),                    intent(in)  :: size_4d
    
    allocate(vals_3d(size_4d(1),size_4d(2),size_4d(4)), &
             vals_1d(size_4d(4)))
    
    vals_3d = sum(vals_4d * vol_4d, dim = 3)
    
    vals_1d = sum(sum(vals_3d, dim = 1), dim = 1)
    
    
  end subroutine sum_layer_data
  
  
  
  subroutine replace_fillvals2zero(vals_in, vals_ot, fillval, count)
    
    real(8), dimension(:,:,:,:),              intent(in)  :: vals_in
    real(8), dimension(:,:,:,:), allocatable, intent(OUT) :: vals_ot
    real(8),                                  intent(in)  :: fillval
    integer, dimension(4),                    intent(in)  :: count
    
    ! iterators
    integer :: i1, i2, i3, i4
    
    allocate(vals_ot(count(1),count(2),count(3),count(4)))
    vals_ot = 0.0_8
    
    do i4 = 1, count(4)
      do i3 = 1, count(3)
        do i2 = 1, count(2)
          do i1 = 1, count(1)
            if(vals_in(i1,i2,i3,i4) .ne. fillval) vals_ot(i1,i2,i3,i4) = &
                                                   vals_in(i1,i2,i3,i4)
          end do
        end do
      end do
    end do
    
  end subroutine replace_fillvals2zero
  
  
  
  subroutine reset_fillvals(vals_ref, vals_in, vals_ot, fillval, count)
    
    real(8), dimension(:,:,:),              intent(in)  :: vals_ref, vals_in
    real(8), dimension(:,:,:), allocatable, intent(OUT) :: vals_ot
    real(8),                                intent(in)  :: fillval
    integer, dimension(3),                  intent(in)  :: count
    
    ! iterators
    integer :: i1, i2, i3
    
    allocate(vals_ot(count(1),count(2),count(3)))
    vals_ot = fillval
    
    do i3 = 1, count(3)
      do i2 = 1, count(2)
        do i1 = 1, count(1)
          if(vals_ref(i1,i2,i3) .ne. fillval) vals_ot(i1,i2,i3) = &
                                               vals_in(i1,i2,i3)
        end do
      end do
    end do
    
  end subroutine reset_fillvals
  
end module process_layer_data