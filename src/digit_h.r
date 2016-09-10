##桁修正##

base$H_HM_pat_rates = dexpd(0.1175137 * base$one, H_HM_pat_rate * base$one, 2013)

base$H_HM_EXP_0_14 = base$H_HM_EXP_0_14 * 1000
base$H_HM_EXP_15_44 = base$H_HM_EXP_15_44 * 1000
base$H_HM_EXP_45_64 = base$H_HM_EXP_45_64 * 1000
base$H_HM_EXP_65_74 = base$H_HM_EXP_65_74 * 1000
base$H_HM_EXP_75_o = base$H_HM_EXP_75_o * 1000

base$H_HM_EXP_SHAKAI = base$H_HM_EXP_SHAKAI / 10

base$H_HC_EXP_SHISETU = base$H_HC_EXP_SHISETU * 1000
base$H_HC_EXP_TIIKI = base$H_HC_EXP_TIIKI * 1000
base$H_HC_EXP_KYOTAKU = base$H_HC_EXP_KYOTAKU * 1000

base$H_HC_EXP_SHISETU_PC = base$H_HC_EXP_SHISETU_PC / 1000
base$H_HC_EXP_TIIKI_PC = base$H_HC_EXP_TIIKI_PC / 1000
base$H_HC_EXP_KYOTAKU_PC = base$H_HC_EXP_KYOTAKU_PC / 1000