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
    finame_in   =   "pdo_sst3_01_60.nc" 
    finame_out  =   "pdo_sst3_01_60_cons_ref.nc" 

    fi_in   =   addfile(finame_in,"r")
    pv      =   fi_in->pv
    pv_o    =   dim_rmvmean_n_Wrap(pv,0)
    printVarSummary(pv_o)

    system("rm -rf "+finame_out)
    fi_out  =   addfile(finame_out,"c")
    setfileoption("nc","Format","LargeFile")
    filedimdef(fi_out,"time",-1,True) 
    fi_out->pv  =   pv_o

end
