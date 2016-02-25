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
    fi_name     =   "pv_time_1948_2014.nc"
    fi_outname  =   "pv_anom_1948_2014.nc" 
    system("rm -rf "+fi_outname)
    fi_out      =   addfile(fi_outname,"c")
    filedimdef(fi_out,"time",-1,True)
    setfileoption("nc","Format","LargeFile")

    if_has_leap =   True 
    if_daily    =   False
    yr_str      =   1948 
    yr_end      =   2014
    num_yr      =   yr_end-yr_str+1
    lon_name    =   "lon"
    lat_name    =   "lat"
    ab_name     =   "block_days"
    pv_name     =   "pv"

    fi          =   addfile(fi_name,"w") 
    pv          =   fi->$pv_name$
    pv_anom     =   pv

    if(if_has_leap) then
        yyyymmdd    =   yyyymmdd_time(yr_str, yr_end, "integer")
        ;com_date    =   mod(yyyymmdd,10000)
    else
        yyyymmdd    =   new(365*num_yr,integer)
        ;com_date    =   new(365*num_yr,integer)
        mon_days    =   (/31,28,31,30,31,30,31,31,30,31,30,31/)
        count = 0
        do i = yr_str,yr_end
        do j = 1, 12
        do k = 1, mon_days(j-1)
            yyyymmdd(count) = i*10000+j*100+k
            ;com_date(count) = j*100+k
            count = count+1
        end do
        end do
        end do
    end if

    if (if_daily) then
        dh  =   1
        real_date   =   yyyymmdd
        com_date    =   mod(real_date,10000)
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
        com_date    =   mod(real_date,1000000)
    end if

    pv&time =   real_date


    num_time    =   dimsizes(real_date)

    do k = 0, num_time-1
        print("Date: "+real_date(k))
        curr_date   =   com_date(k)
        ind_date    =   ind(com_date .eq. curr_date)
        var =   dim_avg_n_Wrap(pv(ind_date,:,:),0)
        pv_anom(k,:,:) = where(ismissing(pv_anom(k,:,:)),pv_anom(k,:,:),pv_anom(k,:,:)-var)
        
        ;print(ind_date)
        delete(ind_date)
        delete(var)
        ;var =   pv(ind_resolve(ind(com_date .eq. com_date(k)),num_time),:,:)
        ;printVarSummary(var)
        ;delete(var)
    end do
    

    fi_out->pv   =  pv_anom 
    ;wks =   gsn_open_wks("eps","block_days")

end
