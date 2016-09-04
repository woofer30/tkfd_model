## 予測値 マクロ


## TFP成長率の伸長
base$M_TFP_GR		= dexpd(base$M_TFP_GR,M_TFP_GR_A*base$one,2013)
## base$M_TFP_GR		= intrpl(base$M_TFP_GR,2013,2018)

## 消費者物価デフレータの伸長

base$M_CPI_GR_T		= base$M_CPI/lag(base$M_CPI,k=-1)-1
base$M_CPI_GR		= dexpd(base$M_CPI_GR_T,M_CPI_GR_A*base$one,2013)
base$M_CPI_GR		= intrpl(base$M_CPI_GR,2013,2018)
base$M_CPI		= acml(base$M_CPI,0*base$one,1+base$M_CPI_GR,2014)

## GDPデフレータの伸長

base$M_PGDP	 	= (base$M_GDP/base$M_GDP_R)*100
base$M_PGDP_GR_T	= base$M_PGDP/lg2(base$M_PGDP)-1
base$M_PGDP_GR		= dexpd(base$M_PGDP_GR_T,
				as.numeric(base$M_PGDP_GR_T[index(base)==2013])
				+base$M_CPI_GR
				-as.numeric(base$M_CPI_GR[index(base)==2013]),2013)
base$M_PGDP		= acml(base$M_PGDP,0*base$one,1+base$M_PGDP_GR,2014)



base$M_WAGE_GR_T	= base$M_WAGE/lag(base$M_WAGE,k=-1)-1

###GDPデフレーターで引き延ばした率
#base$M_WAGE_GR		= dexpd(base$M_WAGE_GR_T,(base$M_TFP_GR)/(1-M_BUNPAI)+base$M_PGDP_GR,2013)

###CPI変化率で引き延ばした率
base$M_WAGE_GR		= dexpd(base$M_WAGE_GR_T,(base$M_TFP_GR)/(1-M_BUNPAI)+base$M_CPI_GR,2013)

base$M_WAGE		= acml(base$M_WAGE,0*base$one,1+base$M_WAGE_GR,2014)



base$M_WAGE_R_GR_T	= base$M_WAGE_GR_T-base$M_PGDP_GR
base$M_WAGE_R_GR	= dexpd(base$M_WAGE_R_GR_T,(base$M_TFP_GR)/(1-M_BUNPAI),2013)
base$M_WAGE_R		= acml(base$M_WAGE_GR-base$M_PGDP_GR,0*base$one,1+base$M_WAGE_R_GR,2014)




base$M_POP_15_64	= base$M_POP_15_44+base$M_POP_45_64
base$M_POP_15_64_GR_T	= base$M_POP_15_64/lag(base$M_POP_15_64,k=-1)-1
base$M_POP_15_64_GR	= dexpd(base$M_POP_15_64_GR_T,M_POP_GR_A*base$one,2013)

base$M_POP_GR_T		= base$M_POP/lag(base$M_POP,k=-1)-1
base$M_POP_GR		= dexpd(base$M_POP_GR_T,M_POP_GR_A*base$one,2013)

base$M_POP_65_o		= base$M_POP_65_74+base$M_POP_75_o


## 実質GDP成長率 実績値
base$M_GDP_R_GR_T 	= base$M_GDP_R/lag(base$M_GDP_R,k=-1)-1
## 実質GDP成長率 伸長
base$M_GDP_R_GR		= dexpd(base$M_GDP_R_GR_T,(base$M_TFP_GR)/(1-M_BUNPAI)+base$M_POP_15_64_GR,2013)
base$M_GDP_R_GR		= intrpl(base$M_GDP_R_GR,2013,2018)
## 実質GDP 水準
base$M_GDP_R		= acml(base$M_GDP_R,0*base$one,1+base$M_GDP_R_GR,2014)

## 名目GDP成長率 実績値
base$M_GDP_GR_T		= base$M_GDP/lag(base$M_GDP,k=-1)-1
## 名目GDP成長率 伸長
base$M_GDP_GR		= dexpd(base$M_GDP_GR_T,base$M_GDP_R_GR+base$M_PGDP_GR,2013)
## 名目GDP 水準
base$M_GDP		= acml(base$M_GDP,0*base$one,1+base$M_GDP_GR,2014)


## 一人あたり 実質GDP成長率 実績値
base$M_GDP_R_PC 	= base$M_GDP_R/base$M_POP
base$M_GDP_R_PC_GR 	= base$M_GDP_R_PC/lag(base$M_GDP_R_PC,k=-1)-1
## 一人あたり 実質GDP 水準
base$M_GDP_R_PC		= acml(base$M_GDP_R,0*base$one,1+base$M_GDP_R_GR,2014)

## 一人あたり 名目GDP成長率 実績値
base$M_GDP_PC 	= base$M_GDP/base$M_POP
base$M_GDP_PC_GR 	= base$M_GDP_PC/lag(base$M_GDP_PC,k=-1)-1
## 一人あたり 名目GDP 水準
base$M_GDP_PC		= acml(base$M_GDP,0*base$one,1+base$M_GDP_GR,2014)




## 利子率
base$M_MARKET_RATE	= dexpd(base$M_MARKET_RATE,base$M_GDP_R_GR+base$M_CPI_GR,2013)
base$M_MARKET_RATE	= intrpl(base$M_MARKET_RATE,2013,2026)
base$M_BOND_RATE	= dexpd(base$M_BOND_RATE,base$M_GDP_GR,2013)
base$M_BOND_RATE	= intrpl(base$M_BOND_RATE,2013,2026)

