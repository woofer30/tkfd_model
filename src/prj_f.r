## 予測値 財政

base$F_FO_RGDP = base$F_FO / base$M_GDP


## f_SSM、F_SHS_FI がcsv内部で二重定義
## F_SNJ_FI.RGDPをF_SNJ_FI_RGDPに変数名修正



##社会保障国庫負担（対GDP)##
## base$F_SHS_FI_RGDP_REF=(base$H_HM_EXP_pub+base$H_HC_EXP_pub+base$P_LIAB_KOKU+base$P_LIAB_WORK)/base$M_GDP
base$F_SHS_FI_RGDP_RES = (base$H_HM_EXP_pub + base$H_HC_EXP_pub + base$P_LIAB_KOKU +
                            base$P_LIAB_WORK) / base$M_GDP


base$F_SHS_FI_RGDP = dexpd(
  base$F_SHS_FI / base$M_GDP,
  as.numeric(base$F_SHS_FI_RGDP[index(base) == 2013])
  + base$F_SHS_FI_RGDP_RES - as.numeric(base$F_SHS_FI_RGDP_RES[index(base) ==
                                                                 2013]) ,
  2013
)
base$F_SHS_FI = base$F_SHS_FI_RGDP * base$M_GDP


##社会保障及び利払い費以外の政府支出（対GDP)##
base$F_SNS_FI_RGDP = base$F_SNS_FI / base$M_GDP
base$F_SNS_FI = dexpd(base$F_SNS_FI_RGDP, base$F_SNS_FI_RGDP[index(base) ==
                                                               2013], 2013) * base$M_GDP



##消費税収（対GDP）##
base$F_XTAX_IW_RGDP  = dexpd(
  base$F_XTAX_IW / base$M_GDP,
  base$F_XTAX_R * as.numeric((base$F_XTAX_IW[index(base) == 2013] / base$M_GDP[index(base) ==
                                                                                 2013]) / base$F_XTAX_R[index(base) == 2013])
  + base$F_DLT * as.numeric((base$F_XTAX_IW[index(base) == 2013] / base$M_GDP[index(base) ==
                                                                                2013]) / base$F_XTAX_R[index(base) == 2013]) * (1 - 1 / 5.4),
  2013
)
base$F_XTAX_IW       = base$F_XTAX_IW_RGDP * base$M_GDP


##消費以外の税収（対GDP）##
base$F_STAX_IW_RGDP = dexpd(base$F_STAX_IW / base$M_GDP,
                            as.numeric(base$F_STAX_IW[index(base) == 2013] / base$M_GDP[index(base) ==
                                                                                          2013]),
                            2013)
base$F_STAX_IW      = base$F_STAX_IW_RGDP * base$M_GDP

##税収(対GDP)##
base$F_TAX_IW_RGDP = base$F_XTAX_IW_RGDP + base$F_STAX_IW_RGDP
base$F_TAX_IW = base$F_XTAX_IW + base$F_STAX_IW

##社会保険料（対GDP)##
base$F_SHL_IW_RGDP = base$F_SHL_IW / base$M_GDP
base$F_SHL_IW = dexpd(base$F_SHL_IW_RGDP, base$F_SHL_IW_RGDP[index(base) ==
                                                               2013], 2013) * base$M_GDP

##利子(受取)
base$F_RT_IW_RGDP = base$F_RT_IW / base$M_GDP
base$F_RT_IW = dexpd(base$F_RT_IW_RGDP, base$F_RT_IW_RGDP[index(base) ==
                                                            2013], 2013) * base$M_GDP



##政府収入（受取利子を除く)（対GDP）を計算する##
base$F_FI_RGDP = base$F_XTAX_IW_RGDP + base$F_STAX_IW_RGDP + base$F_SHL_IW_RGDP
base$F_FI = base$F_FI_RGDP * base$M_GDP

##政府支出（支払利子を除く)（対GDP）を計算する  ##   - base$F_RT_FI_RGDP
base$F_FO_RGDP = base$F_SHS_FI_RGDP + base$F_SNS_FI_RGDP
base$F_FO = base$F_FO_RGDP * base$M_GDP

##プライマリー収支（対GDP）を計算する##
##ステップ1：「税収等－政府支出」##
base$F_PRB = base$F_FI - base$F_FO + base$F_PRB_FIX_RGDP * base$M_GDP

##ステップ2：プライマリー収支に受取利子分だけを加えた変数を作成
base$F_PRB_PLUS_RT_IW	=	base$F_PRB + base$F_RT_IW

##ステップ3：求められた利払い費とプライマリー収支から、「－プライマリー収支＋利払い費＋前年度の公債等残高」として、毎年度の公債等残高を計算する。##
##政府債務残高##   こちらの変数には平均残存期間等の調整が必要
base$F_SSM = acml(base$F_SSM,
                  -base$F_PRB_PLUS_RT_IW,
                  1 + base$M_BOND_RATE - F_BOND_SPREAD,
                  2014)
base$F_SSM_RGDP = base$F_SSM / base$M_GDP


##利子（支払)
base$F_RT_FI_RGDP = dexpd(base$F_RT_FI,
                          (base$M_BOND_RATE - F_BOND_SPREAD) * lg2(base$F_SSM),
                          2013) / base$M_GDP
base$F_RT_FI = base$F_RT_FI_RGDP * base$M_GDP

##財政収支
base$F_FB = base$F_PRB - base$F_RT_FI + base$F_RT_IW
base$F_FB_RGDP = base$F_FB / base$M_GDP

##rm("base$F_RBR_FI")
##base$F_RBR_FI = (base$M_BOND_RATE-F_BOND_SPREAD)*lg2(base$F_SSM)
