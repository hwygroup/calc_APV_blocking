program main
    use nc_read_write_interface
    use time_manager
    use array_manager
    use area_wgt_manager
    use nml_manager
    implicit none
    
    
    character(len = 500) :: his_fi
    character(len = 500) :: namelist_file_name 
    integer :: num_six_hours
    integer :: num_days
    integer, allocatable, dimension(:) :: date

    logical, parameter :: if_restart    =   .false. 
    integer  :: start_ind, end_ind


    
    integer :: case_hours(200000)

    real (r8), allocatable, dimension(:)     :: lon, lat
    real (r8), allocatable, dimension(:,:)   :: pv_mean, pv, apv, areawgt
    real (r8), allocatable, dimension(:,:,:) :: pv_mean0,pv0 
    

    !real (r8) :: lon(num_lon)
    !real (r8) :: lat(num_lat)
    !real (r8) :: pv_mean(num_lon,num_lat), pv_mean0(num_lon,num_lat,1)
    !real (r8) :: pv(num_lon,num_lat), pv0(num_lon,num_lat,1)
    !real (r8) :: apv(num_lon,num_lat)
    !real (r8) :: areawgt(num_lon,num_lat)

    type (atmos_block) :: curr_state, prev_state

    character(len = 500) :: out_fi, out_fi1
    character(len = 500) :: date_now_char

    integer :: i, j, k, kk 
    integer :: date_now, max_eve

    integer :: num_inds
    integer, allocatable, dimension(:) :: lon_inds, lat_inds

    integer, allocatable, dimension(:) :: pts, eves
    logical :: folder_exist


    
    !call get_command_argument (1, namelist_file_name)
    !open (10, file = namelist_file_name)
    !read(10,nml = namelist_for_track_AB)
    !close(10)

    call read_nml

    inquire(file=trim(out_dir),EXIST=folder_exist)
    
    if (folder_exist) then
        print *, "out_folder_exist"
        stop
    else
        call system("mkdir -p "//trim(out_dir))
    end if

    allocate(lon(num_lon),lat(num_lat))
    allocate(pv_mean(num_lon,num_lat),pv(num_lon,num_lat))
    allocate(apv(num_lon,num_lat),areawgt(num_lon,num_lat))
    allocate(pv_mean0(num_lon,num_lat,1),pv0(num_lon,num_lat,1))

    his_fi  = trim(out_dir)//"his.txt"

    case_hours  =   0
    
    if (.not. if_daily) then
        print *, yr_str_ref, yr_end_ref
        num_six_hours    =   six_hours_of_years(yr_str_ref,yr_end_ref)
        allocate(date(1:num_six_hours))
        call date_yymmddhh_years_six_hours(yr_str_ref,yr_end_ref,num_six_hours,date)
        print *, date(1), date(num_six_hours)
        start_ind   =   index_a_in_b(start_date,date,num_six_hours)
        print *, start_ind
        end_ind     =   index_a_in_b(end_date,date,num_six_hours)
        print *,  "num_six_hours: ", num_six_hours
    else
        print *, yr_str_ref, yr_end_ref
        num_days    =   days_of_years(yr_str_ref,yr_end_ref)
        allocate(date(1:num_days))
        call date_yymmdd_years(yr_str_ref,yr_end_ref,num_days,date)
        print *, date(1), date(num_days)
        start_ind   =   index_a_in_b(start_date,date,num_days)
        print *, start_ind
        end_ind     =   index_a_in_b(end_date,date,num_days)
        print *, end_ind
        print *,  "num_days: ", num_days
    end if
        print *, "start_ind: ", start_ind
        print *, "end_ind: ",end_ind

    call nc_read_write_interface_read_var (lon,pv_fi,lon_name,[1],[num_lon],[num_lon],1) 
    call nc_read_write_interface_read_var (lat,pv_fi,lat_name,[1],[num_lat],[num_lat],1) 
    if (.not. if_anomaly) then
        call nc_read_write_interface_read_var (pv_mean0,mean_pv_fi,pv_name,[1,1,1],[num_lon,num_lat,1],[num_lon,num_lat,1],3) 
        pv_mean = pv_mean0(:,:,1)
        print*,"Max and Min of Mean PV: ", maxval(pv_mean), " ", minval(pv_mean)
    end if
    areawgt = create_areawgt_latlon (lat, lon, num_lat, num_lon)
    write(*,"(A15E10.4)") "Total Area: ",sum(areawgt)
    
    looptime:do k = start_ind, end_ind
        date_now    =   date(k)
        if (if_daily) then
            write(date_now_char,"(I8)") date_now
        else
            write(date_now_char,"(I10)") date_now
        end if
        out_fi      = trim(out_dir)//trim(date_now_char)//".nc"
        out_fi1     = trim(out_dir)//trim(date_now_char)//"_1.nc"
        print*,"Curr Date: ",date_now
        call atmos_block_ini(curr_state)
        curr_state%curr_date     =   date(k)
        if (k == start_ind) then
            call atmos_block_ini(prev_state)
            prev_state%max_eve       =   0
            prev_state%num_eve       =   0
            curr_state%is_initial    =   .true. 
            curr_state%is_daily      =   if_daily 
        else
            curr_state%is_initial    =   .false.
            curr_state%is_daily      =   if_daily
        end if
        curr_state%max_eve       =   prev_state%max_eve
        curr_state%num_eve       =   0 
        call nc_read_write_interface_read_var (pv0,pv_fi,"pv",[1,1,k],[num_lon,num_lat,1],[num_lon,num_lat,1],3) 
        pv  =   pv0(:,:,1)
        if (if_anomaly) then
            apv =   pv
        else
            apv =   pv - pv_mean
        end if
        call track_2d_apv(apv,areawgt,lon,lat,num_lon,num_lat,curr_state,prev_state)
        prev_state  =   curr_state
        call nc_read_write_interface_delete_file (out_fi)        
        call nc_read_write_interface_create_file (out_fi)
        call nc_read_write_interface_write_dim (lon,out_fi,"lon","lon","degrees_east",num_lon)
        call nc_read_write_interface_write_dim (lat,out_fi,"lat","lat","degrees_north",num_lat)
        call nc_read_write_interface_write_scalar(curr_state%num_eve,out_fi,"num_eve","num_eve","num_eve",10000000)
        if (curr_state%num_eve == 0)cycle looptime 

        allocate(pts(1:curr_state%num_pts))
        allocate(eves(1:curr_state%num_eve))
        do kk = 1, curr_state%num_pts
            pts(kk) = kk
        end do
        do kk = 1, curr_state%num_eve 
            eves(kk)    =   kk
            case_hours(curr_state%ini_case(kk)) = curr_state%hrs(kk)
        end do
        call nc_read_write_interface_delete_file (out_fi1)        
        call nc_read_write_interface_create_file (out_fi1)
        call nc_read_write_interface_write_scalar(curr_state%num_eve,out_fi1,"num_eve","num_eve","num_eve",10000000)
        call nc_read_write_interface_write_scalar(curr_state%max_eve,out_fi1,"max_eve","max_eve","max_eve",10000000)
        call nc_read_write_interface_write_scalar(curr_state%curr_date,out_fi1,"date","date","days",10000000)
        call nc_read_write_interface_write_scalar(curr_state%num_pts,out_fi1,"num_pts","num_pts","num_pts",10000000)
        call nc_read_write_interface_write_dim (pts,out_fi1,"pts","pts","pts",curr_state%num_pts)
        call nc_read_write_interface_write_dim (eves,out_fi1,"eves","eves","eves",curr_state%num_eve)
        call nc_read_write_interface_write_var (curr_state%lons(1:curr_state%num_pts),&
                        out_fi1,["pts"],"lons","lons","lons",100000,[curr_state%num_pts],1)
        call nc_read_write_interface_write_var (curr_state%lats(1:curr_state%num_pts),&
                        out_fi1,["pts"],"lats","lats","lats",100000,[curr_state%num_pts],1)
        call nc_read_write_interface_write_var (curr_state%ins(1:curr_state%num_eve),&
                        out_fi1,["eves"],"ins","ins","ins",100000,[curr_state%num_eve],1)
        call nc_read_write_interface_write_var (curr_state%ine(1:curr_state%num_eve),&
                        out_fi1,["eves"],"ine","ine","ine",100000,[curr_state%num_eve],1)
        call nc_read_write_interface_write_var (curr_state%hrs(1:curr_state%num_eve),&
                        out_fi1,["eves"],"hrs","hrs","hrs",100000,[curr_state%num_eve],1)
        call nc_read_write_interface_write_var (curr_state%area(1:curr_state%num_eve),&
                        out_fi1,["eves"],"area","area","area",1.0d+20,[curr_state%num_eve],1)
        call nc_read_write_interface_write_var (curr_state%ini_case(1:curr_state%num_eve),&
                        out_fi1,["eves"],"ini_case","ini_case","ini_case",100000,[curr_state%num_eve],1)
        call nc_read_write_interface_write_var (curr_state%pre_case(1:curr_state%num_eve),&
                        out_fi1,["eves"],"pre_case","pre_case","pre_case",100000,[curr_state%num_eve],1)

        deallocate(pts)
        deallocate(eves)
    end do looptime
    
    open(unit=10, file=his_fi)
    do kk = 1, curr_state%max_eve
        write(10,"(I10,I10)") kk,case_hours(kk) 
    end do
    close(10)
    stop
end program main
