module nml_manager
    use precision_manager
    implicit none
    integer :: yr_str_ref    =   1948
    integer :: yr_end_ref    =   2014
    integer :: start_date    =   19480101
    integer :: end_date      =   20141231
    integer :: num_lon   =   144
    integer :: num_lat   =   73
    character(len = 500) :: mean_pv_fi  =   "/nfs9/home/hwy/Atmos_Block/Data/PV_150_500mb_avg_dims_daily/pv_1948_2014.nc"
    character(len = 500) :: pv_fi       =   "/nfs9/home/hwy/Atmos_Block/Data/PV_150_500mb_avg_dims_daily/pv_time_1948_2014.nc"
    character(len = 500) :: out_dir     =   "/nfs9/home/hwy/track_AB_general/data_out/d05/" 
    logical :: if_daily      =   .true.
    logical :: if_has_leap   =   .true.
    logical :: if_anomaly    =   .true.
    logical :: if_defined_overlap_ratio  =   .true.
    real(r8) :: overlap_ratio    =   0.7d0
    real(r8) :: apv_cr           =   -1.2d0
    real(r8) :: area_cr          =   1.8
    character (len = 500) :: lon_name   =   "lon"
    character (len = 500) :: lat_name   =   "lat"
    character (len = 500) :: pv_name    =   "pv"

    namelist /namelist_for_track_AB/ yr_str_ref, &
                                     yr_end_ref, &
                                     start_date, &
                                     end_date,   & 
                                     num_lon,    &
                                     num_lat,    &
                                     mean_pv_fi, &
                                     pv_fi,      &
                                     out_dir,    &
                                     if_daily,   &
                                     if_has_leap,&
                                     if_anomaly, &
                                     if_defined_overlap_ratio,&
                                     overlap_ratio,&
                                     apv_cr,&
                                     area_cr,&
                                     lon_name,   &
                                     lat_name,   &
                                     pv_name    

contains
    subroutine read_nml()
        implicit none
        character (len = 500) :: namelist_file_name
        call get_command_argument (1, namelist_file_name)
        open (10, file = namelist_file_name)
        read(10,nml = namelist_for_track_AB)
        close(10)
    end subroutine read_nml
    

end module nml_manager
