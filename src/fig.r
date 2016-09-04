## 作図用データ整形
## 財政：PB、税収、社会保障料収入、歳出(利払い費除く)、歳出(利払・社会保障費除く)、歳出(社会保障)、利払、(債務残高)
## 年金：受け取り保険料、支払保険料、国庫負担、積立金
## 医療介護：保険料、給付、国庫負担

list1 = c("M_GDP","M_GDP_R","M_GDP_PC","M_GDP_R_PC","M_PGDP_GR","M_GDP_GR","M_GDP_R_GR","M_GDP_PC_GR","M_GDP_R_PC_GR","M_CPI_GR","M_MARKET_RATE","M_BOND_RATE","F_FB","F_PRB","F_FI","F_SHL_IW","F_FO","F_SNS_FI","F_SHS_FI","F_RT_FI","F_SSM","H_HM_de_EXP","H_HM_EXP_pub","H_HC_de_EXP","H_HC_EXP_pub","P_AREV_KOKU","P_AREV_WORK","P_AEXP_KOKU","P_AEXP_WORK","P_LIAB_KOKU","P_LIAB_WORK","P_RESERVE_KOKU","P_RESERVE_WORK")


# 2013年以前カット

for(j in list1) { 
eval(parse(text=paste("base$", j," = dexpd(NA*base$one, base$", j ,", 2012)" ,sep = "")))
}

# 2051年以降カット

for(j in list1) { 
eval(parse(text=paste("base$", j," = dexpd(base$", j ,", NA*base$one, 2050)" ,sep = "")))
}

# 総生産比計算(100倍してパーセント表記)

for(j in list1) { 
eval(parse(text=paste("base$", j,"_RP = base$", j ,"/base$M_GDP*100" ,sep = "")))
}


base$M_GDP_GR_RP = base$M_GDP_GR *100
base$M_GDP_R_GR_RP = base$M_GDP_R_GR *100
base$M_GDP_PC_GR_RP = base$M_GDP_PC_GR *100
base$M_GDP_R_PC_GR_RP = base$M_GDP_R_PC_GR *100
base$M_PGDP_GR_RP = base$M_PGDP_GR *100
base$M_CPI_GR_RP = base$M_CPI_GR *100
base$M_MARKET_RATE_RP = base$M_MARKET_RATE *100
base$M_BOND_RATE_RP = base$M_MARKET_RATE *100

# 変数リストの変数変換（未完了）
list2=list1
for(j in 1:length(list1)) { 
list2[j]=paste("base$",list1[j], sep = "")
}


# 変数リスト（暫定）
listx = cbind(base$year,base$M_GDP,base$M_GDP_R,base$M_GDP_PC,base$M_GDP_R_PC,base$M_GDP_GR_RP,base$M_GDP_R_GR_RP,base$M_GDP_PC_GR_RP,base$M_GDP_R_PC_GR_RP,base$M_PGDP_GR_RP,base$M_CPI_GR_RP,base$M_MARKET_RATE_RP,base$M_BOND_RATE_RP,base$F_FB,base$F_PRB,base$F_FI,base$F_SHL_IW,base$F_FO,base$F_SNS_FI,base$F_SHS_FI,base$F_RT_FI,base$F_SSM,base$H_HM_de_EXP,base$H_HM_EXP_pub,base$H_HC_de_EXP,base$H_HC_EXP_pub,base$P_AREV_KOKU,base$P_AREV_WORK,base$P_AEXP_KOKU,base$P_AEXP_WORK,base$P_LIAB_KOKU,base$P_LIAB_WORK,base$P_RESERVE_KOKU,base$P_RESERVE_WORK)


# データのアウトプット
write.csv(base, file = 'outdata.csv', row.names = TRUE)
write.csv(listx, file = 'outdata2.csv', row.names = TRUE)






