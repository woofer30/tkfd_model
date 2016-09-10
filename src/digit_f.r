## シナリオ変数 財政

base$DMY_F_DLT_MID = ifelse(index(base) > 2016, ifelse(index(base) == F_DLT_Y0_MID, 1, 0), 0)

base$F_PRB_FIX_RGDP = dexpd(0 * base$one, 0.01 * base$one , 2013)
base$F_DLT = dexpd(base$F_DLT,
                   base$F_DLT + as.numeric(F_DLT_P1) * base$one,
                   as.numeric(F_DLT_Y1) - 1)
base$F_DLT = dexpd(
  base$F_DLT,
  base$F_DLT + as.numeric(F_DLT_P0) * base$one - 0.5 * as.numeric(F_DLT_P0) *
    base$DMY_F_DLT_MID,
  as.numeric(F_DLT_Y0) - 1
)
base$F_DLT = dexpd(base$F_DLT,
                   base$F_DLT + as.numeric(F_DLT_P) * base$one,
                   as.numeric(F_DLT_Y) - 1)
