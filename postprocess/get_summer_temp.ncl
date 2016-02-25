load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
undef("index_a_in_b")
function index_a_in_b(a:numeric,b:numeric)
begin
    num_pt  =   dimsizes(b)
    do k = 0, num_pt-1
        if (a .eq. b(k)) then
            index_a =   k
            return(index_a)
        end if
    end do
end
begin
    case_name   =   "pdo_sst4_var" 
    if_has_leap =   False
    if_daily    =   False 
    yr_str      =   1901
    yr_end      =   1960
    num_yr      =   yr_end-yr_str+1
    ;ref_fi_name =   "../data_in/20cr_1d/pv_time_1900_2011.nc" 
    ;ref_fi_name =   "../data_in/era40_6h/pv_1958_2001_ct.nc" 
    ;ref_fi_name =   "../data_in/era_20c/pv_0001_0112.nc"
    ref_fi_name =   "../data_in/cam_pdo_sst4_zmoff/apv_pdo_sst4_zmoff_dims_0001_0040.nc"
    ;ref_fi_name =   "../data_in/gfdl/apv_0001_0200.nc"
    ;ref_fi_name =   "../data_in/ncep_6h/pv_time_1948_2014.nc" 
    ;ref_fi_name =   "../data_in/gamil_amip/gamil_amip_1979_2008.nc"
    lon_name    =   "lon"
    lat_name    =   "lat"
    ab_name     =   "block_days"
    pv_name     =   "pv"

    if(if_has_leap) then
        yyyymmdd    =   yyyymmdd_time(yr_str, yr_end, "integer")
    else
        yyyymmdd    =   new(365*num_yr,integer)
        mon_days    =   (/31,28,31,30,31,30,31,31,30,31,30,31/)
        count = 0
        do i = yr_str,yr_end
        do j = 1, 12
        do k = 1, mon_days(j-1)
            yyyymmdd(count) = i*10000+j*100+k
            count = count+1
        end do
        end do
        end do
    end if

    if (if_daily) then
        dh  =   1
        real_date   =   yyyymmdd
    else
        dh  =   0.25
        yyyymmddhh  =   new(dimsizes(yyyymmdd)*4, integer)
        do k = 0, dimsizes(yyyymmdd)-1
            yyyymmddhh(k*4)   = yyyymmdd(k)*100 
            yyyymmddhh(k*4+1) = yyyymmdd(k)*100+6 
            yyyymmddhh(k*4+2) = yyyymmdd(k)*100+12
            yyyymmddhh(k*4+3) = yyyymmdd(k)*100+18
        end do
        real_date   =   yyyymmddhh
    end if
    tot_date    =   dimsizes(real_date)
    tot_days    =   dimsizes(yyyymmdd) 
    final_fi    =   addfile("../data_out/"+case_name+"/"+real_date(tot_date-1)+"_1.nc","r")
    max_eve     =   final_fi->max_eve
    data        =   readAsciiTable("../data_out/"+case_name+"/"+"his.txt", 2, "integer", 0)
    life        =   data(:,1)
    
    print("tot_days: "+tot_days)
    print("tot_date: "+tot_date)
    print("max eve: "+max_eve)


    ref_fi  =   addfile(ref_fi_name,"r")
    pv      =   ref_fi->$pv_name$(0:num_yr-1,:,:)
    block_days  =   pv
    block_days  =   0.0
    printVarSummary(pv)

    lon =   ref_fi->$lon_name$
    lat =   ref_fi->$lat_name$

    do year = yr_str, yr_end
        start_date  =   year*10000+601
        end_date    =   year*10000+901
        if(.not.if_daily) then
            start_date  =   start_date*100
            end_date    =   end_date*100
        end if
        start_ind   =   index_a_in_b(start_date,real_date)
        printVarSummary(index_a_in_b(start_date,real_date))
        end_ind     =   index_a_in_b(end_date,real_date)
        printVarSummary(index_a_in_b(end_date,real_date))
        print(start_ind)
        print(end_ind)
    do k = start_ind, end_ind-1 
        date_now    =   real_date(k)
        print("Curr_date: "+date_now)
        curr_fi0     =   addfile("../data_out/"+case_name+"/"+date_now+".nc","r")
        num_min     =   curr_fi0->num_eve
        if (num_min .gt. 0 ) then
            curr_fi     =   addfile("../data_out/"+case_name+"/"+date_now+"_1.nc","r")
            do j = 0, num_min-1
                case    =   curr_fi->ini_case(j)
                ins     =   curr_fi->ins(j)-1 
                ine     =   curr_fi->ine(j)-1
                hrs     =   curr_fi->hrs(j)
                ;life    =   hrs_eve(case-1) 
                if (life(case-1) .ge. 120) then
                    lons    =   curr_fi->lons(ins:ine)
                    lats    =   curr_fi->lats(ins:ine)
                    num_pt  =   dimsizes(lons)
                    print("Case: "+case+" Num_eve: "+(j+1)+" life: "+life(case-1)+" hrs:"+hrs)
                    do i = 0, num_pt-1
                        jj = lats(i)-1 
                        ii = lons(i)-1 
                        block_days(year-yr_str,jj,ii) = block_days(year-yr_str,jj,ii) + dh 
                    end do
                    delete(lons)
                    delete(lats)
                end if
            end do
            ;printMinMax(block_days,0)
        end if
    end do
    end do
    

    time    =   ispan(yr_str,yr_end,1)
    block_days!0    =   "time"
    block_days&time =   time
    out_fi_name =   "summer_"+yr_str+"_"+yr_end+"_"+case_name+".nc"
    system("/bin/rm -f "+ out_fi_name)   ; remove any pre-existing file
    ncdf = addfile(out_fi_name ,"c")  ; open output netCDF file
    ncdf->block_days   =  block_days 
    ;wks =   gsn_open_wks("eps","block_days")



end
