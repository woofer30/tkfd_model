


##対平成16年度比賃金変化率の計算


base$P_RNWAGE_H16 = dexpd(base$P_RNWAGE_H16,
                          base$M_WAGE / as.numeric(base$M_WAGE["2004"]) -
                            1,
                          2012)
##被保険者数の計算
base$P_NPOP_NI = base$P_NPOP_NI_KOSEI + base$P_NPOP_NI_KOKKA + base$P_NPOP_NI_CHIHOU + base$P_NPOP_NI_SHIGAKU

base$P_NPOP_SAN = dexpd2(
  base$P_NPOP_SAN,
  base$P_NPOP_SAN_KOSEI + base$P_NPOP_SAN_KOKKA + base$P_NPOP_SAN_CHIHOU + base$P_NPOP_SAN_SHIGAKU,
  1989
)

##被保険者数変化率の計算
base$P_RNPOP_P = dexpd(
  base$P_RNPOP_P,
  diff(base$P_NPOP_ICHI + base$P_NPOP_NI + base$P_NPOP_SAN) /
    (base$P_NPOP_ICHI + base$P_NPOP_NI + base$P_NPOP_SAN),
  2008
)

##足元の保険料収入の計算
base$P_REV_KOKKA = base$P_REV_KOKKA_KAKE * 2
base$P_REV_CHIHOU = base$P_REV_CHIHOU_KAKE * 2


## マクロ経済スライドを考慮した名目可処分所得の計算

#平均被保険者数減少率の計算
for (i in 2013:2110)
{
  base$P_ARNPOP_P[paste(i)] = mean(base$P_RNPOP_P[paste(i - 4)],
                                   base$P_RNPOP_P[paste(i - 3)],
                                   base$P_RNPOP_P[paste(i - 2)])
}

#変化率のラグを取る
base$P_WAGE_GR_MACRO_WORK = lg2(base$M_WAGE_GR)
base$P_WAGE_GR_MACRO_KISO = lg2(base$M_WAGE_GR)


#場合分けを行いながら、変化率にマクロ経済スライドを反映
for (m in 2013:P_SLIDE_WORK_END) {
  base$P_WAGE_GR_MACRO_WORK[paste(m)]  = ifelse(
    as.numeric(-base$P_ARNPOP_P[paste(m)] + 0.003) < as.numeric(base$M_WAGE_GR[paste(m -
                                                                                       1)]),
    base$M_WAGE_GR[paste(m - 1)] +
      as.numeric(base$P_ARNPOP_P[paste(m)] - 0.003),
    ifelse(as.numeric(base$M_WAGE_GR[paste(m -
                                             1)]) > 0, 0,
           base$M_WAGE_GR[paste(m -
                                  1)])
  )
}

for (m in 2013:P_SLIDE_KISO_END) {
  base$P_WAGE_GR_MACRO_KISO[paste(m)]  = ifelse(
    as.numeric(-base$P_ARNPOP_P[paste(m)] + 0.003) < as.numeric(base$M_WAGE_GR[paste(m -
                                                                                       1)]),
    base$M_WAGE_GR[paste(m - 1)] +
      as.numeric(base$P_ARNPOP_P[paste(m)] - 0.003),
    ifelse(as.numeric(base$M_WAGE_GR[paste(m -
                                             1)]) > 0, 0,
           base$M_WAGE_GR[paste(m -
                                  1)])
  )
}

#マクロ経済スライドを考慮した変化率で引き延ばす

base$P_WAGE_WORK = acml(base$M_WAGE, 0 * base$one, 1 + base$P_WAGE_GR_MACRO_WORK, 2013)
base$P_WAGE_KISO = acml(base$M_WAGE, 0 * base$one, 1 + base$P_WAGE_GR_MACRO_KISO, 2013)


##8年平均名目可処分所得の計算
for (i in 1980:2101)
{
  base$P_WAGE_A[paste(i + 7)] = mean(
    base$P_WAGE_WORK[paste(i)],
    base$P_WAGE_WORK[paste(i + 1)],
    base$P_WAGE_WORK[paste(i + 2)],
    base$P_WAGE_WORK[paste(i + 3)],
    base$P_WAGE_WORK[paste(i + 4)],
    base$P_WAGE_WORK[paste(i + 5)],
    base$P_WAGE_WORK[paste(i + 6)],
    base$P_WAGE_WORK[paste(i + 7)]
  )
}



## 国民年金純給付費		base$P_NEXP_KOKU
base$P_NEXP_KOKU =	base$P_EXP_KOKU - base$P_GRA_KOKU
base$P_NEXP_KOKU["2013"] = mean(base$P_NEXP_KOKU, na.rm = T)


base$P_NEXP_KOKU =	acml(base$P_NEXP_KOKU,
                        0 * base$one,
                        1 + base$M_POP_GR,
                        2014)

##国民年金その他支出
base$P_EXP_OTH_KOKU =  acml(base$P_EXP_OTH_KOKU,
                            0 * base$one,
                            1 + base$M_POP_GR,
                            2015)

##被用者年金給付費　base$P_NEXP_WORK

base$P_EXP_CHIHOU =	base$P_EXP_CHIHOU_TAISHOKU + base$P_EXP_CHIHOU_SHOGAI +
  base$P_EXP_CHIHOU_IZOKU

base$P_EXP_SHIGAKU =	base$P_EXP_SHIGAKU_TAISHOKU + base$P_EXP_SHIGAKU_SHOGAI +
  base$P_EXP_SHIGAKU_IZOKU

base$P_NEXP_WORK =	(
  base$P_EXP_KOSEI + base$P_EXP_KOKKA + base$P_EXP_CHIHOU + base$P_EXP_SHIGAKU
  - base$P_GRA_KOSEI - base$P_GRA_KOKKA - base$P_GRA_CHIHOU -
    base$P_GRA_SHIGAKU
)

base$P_NPOP_JUKYU_WORK =	(
  base$P_NPOP_JUKYU_KOSEI +
    base$P_NPOP_JUKYU_KOKKA +
    base$P_NPOP_JUKYU_CHIHOU +
    base$P_NPOP_JUKYU_SHIGAKU
)


base$P_NPOP_JUKYU_WORK = dexpd(base$P_NPOP_JUKYU_WORK,
                               exp(-5.245880 +  1.52787 * log(base$M_POP_65_o)),
                               2013)


base$P_NEXP_WORK = dexpd(base$P_NEXP_WORK,
                         exp(
                           -6.4159025 + 0.5247495 * log(base$P_NPOP_JUKYU_WORK) + 0.8864705 * log(base$P_WAGE_A)
                         ),
                         2013)


##被用者年金その他支出
base$P_EXP_OTH_WORK =  acml(base$P_EXP_OTH_WORK,
                            0 * base$one,
                            1 + base$M_POP_GR,
                            2015)


##　基礎年金給付費 base$P_NEXP_KISO


base$P_NPOP_JUKYU_P = base$P_NPOP_JUKYU_KISO + base$P_NPOP_JUKYU_WORK


base$P_NPOP_JUKYU_P =  dexpd(base$P_NPOP_JUKYU_P,
                             exp(-3.804379 + 1.445686 * log(base$M_POP_65_o)),
                             2013)


base$P_EXP_KISO =  dexpd(
  base$P_EXP_KISO,
  exp(
    -10.4560219952666 + 0.946088920842381 * log(base$P_NPOP_JUKYU_P) + 0.785966661933845 *
      log(base$P_WAGE_KISO)
  ),
  2013
)


## 特別国庫負担


base$P_KENSHO_EXCEPT_LIAB_KOKU = base$P_KENSHO_LIAB_KOKU - base$P_KENSHO_EXP_KOKU_KISO *
  0.5
base$P_REXCEPT_LIAB = base$P_KENSHO_EXCEPT_LIAB_KOKU / base$P_KENSHO_EXP_KISO

base$P_EXCEPT_LIAB = base$P_EXP_KISO * base$P_REXCEPT_LIAB

## 国民年金基礎年金拠出金　base$P_EXP_KOKU_KISO

base$P_EXP_KOKU_KISO =  dexpd(
  base$P_EXP_KOKU_KISO,
  (base$P_EXP_KISO - base$P_EXCEPT_LIAB) / (base$P_NPOP_ICHI_SANTEI +
                                              base$P_NPOP_NI + base$P_NPOP_SAN) * base$P_NPOP_ICHI_SANTEI,
  2013
)


##被用者年金基礎年金拠出金 base$P_EXP_WORK_KISO

base$P_EXP_WORK_KISO =	dexpd(
  base$P_EXP_WORK_KISO,
  (base$P_EXP_KISO - base$P_EXCEPT_LIAB) / (base$P_NPOP_ICHI_SANTEI +
                                              base$P_NPOP_NI + base$P_NPOP_SAN) * (base$P_NPOP_NI + base$P_NPOP_SAN),
  2013
)



##国民年金国庫負担金

base$P_LIAB_KOKU =  dexpd(base$P_LIAB_KOKU,
                          base$P_EXP_KOKU_KISO * 0.5 + base$P_EXCEPT_LIAB,
                          2013)

##被用者年金国庫負担金
base$P_LIAB_WORK =	base$P_EXP_WORK_KISO * 0.5


##国民年金総支出
base$P_AEXP_KOKU =	base$P_NEXP_KOKU + base$P_EXP_KOKU_KISO + base$P_EXP_OTH_KOKU

##被用者年金総支出
base$P_AEXP_WORK =	base$P_NEXP_WORK + base$P_EXP_WORK_KISO + base$P_EXP_OTH_WORK


##国民年金保険料収入 base$P_REV_KOKU

base$P_REV_KOKU = dexpd(
  base$P_REV_KOKU,
  (base$P_NPOP_ICHI * 1000 * (1 - base$P_RMENJO) *
     base$P_RNOHU) * base$P_MPRE_KOKU * (1 + base$P_RNWAGE_H16) * 12,
  2013
)



##被用者年金保険料収入 base$P_REV_WORK


base$P_REV_WORK = base$P_REV_KOSEI + base$P_REV_KOKKA + base$P_REV_CHIHOU +
  base$P_REV_SHIGAKU

#base$P_REWARD = acml(base$P_REWARD, 0*base$one, 1+base$M_WAGE_GR ,2015)

base$P_REV_WORK =  dexpd(base$P_REV_WORK,
                         base$P_REWARD * (1 + base$M_WAGE_GR) * base$P_RPRE_KOSEI,
                         2013)

#base$P_REV_WORK =  dexpd(base$P_REV_WORK,
#                         base$P_REWARD*base$P_RPRE_KOSEI,
#                         2013)


##国民年金その他収入
base$P_R_OTH_KOKU = dexpd (base$P_R_OTH_KOKU,
                           base$P_KENSHO_REV_OTH_KOKU / base$P_KENSHO_REV_KOKU,
                           2013)

base$P_REV_OTH_KOKU = dexpd(base$P_REV_OTH_KOKU, base$P_REV_KOKU * base$P_R_OTH_KOKU, 2013)


##被用者年金その他収入

base$P_R_OTH_WORK = dexpd (base$P_R_OTH_WORK,
                           base$P_KENSHO_REV_OTH_WORK / base$P_KENSHO_REV_WORK,
                           2013)

P_R_REV_OTH_WORK = mean(base$P_KENSHO_REV_WORK, na.rm = T)

base$P_REV_OTH_WORK = dexpd(base$P_REV_OTH_WORK, base$P_REV_WORK * base$P_R_OTH_WORK, 2013)


##国民年金総収入 base$P_AREV_KOKU
base$P_AREV_KOKU =  base$P_REV_KOKU + base$P_LIAB_KOKU + base$P_REV_OTH_KOKU

##被用者年金総収入 base$P_AREV_WORK
base$P_AREV_WORK =	base$P_REV_WORK + base$P_LIAB_WORK + base$P_REV_OTH_WORK


##国民年金収支(運用収入なし)
base$P_BA_KOKU =  base$P_AREV_KOKU - base$P_AEXP_KOKU

##被用者年金収支(運用収入なし)
base$P_BA_WORK =	base$P_AREV_WORK - base$P_AEXP_WORK


##利回りの計算
for (i in 2006:2013) {
  base$P_PREMIUM[paste(i)] =  base$M_MARKET_RATE[paste(i)] - base$P_TOVER[paste(i)]
}

PREMIUM = mean(base$P_PREMIUM, na.rm = T)

base$P_TOVER = dexpd(base$P_TOVER, base$M_MARKET_RATE - base$one * PREMIUM , 2012)


##国民年金運用収入・積立金

for (i in 2014:2110) {
  base$P_INCOME_KOKU[paste(i)] = (as.numeric(base$P_RESERVE_KOKU[paste(i -
                                                                         1)]) +
                                    as.numeric(base$P_BA_KOKU[paste(i)]) *
                                    0.5) * as.numeric(base$P_TOVER[paste(i)])
  base$P_RESERVE_KOKU[paste(i)] = as.numeric(base$P_INCOME_KOKU[paste(i)]) +
    as.numeric(base$P_RESERVE_KOKU[paste(i - 1)] +
                 as.numeric(base$P_BA_KOKU[paste(i)]))
  base$P_AREV_KOKU[paste(i)] = base$P_AREV_KOKU[paste(i)] + base$P_INCOME_KOKU[paste(i)]
  base$P_ABA_KOKU[paste(i)] = base$P_BA_KOKU[paste(i)] + base$P_INCOME_KOKU[paste(i)]
}


##被用者年金運用収入・積立金

for (i in 2014:2110) {
  base$P_INCOME_WORK[paste(i)] = (as.numeric(base$P_RESERVE_WORK[paste(i -
                                                                         1)]) +
                                    as.numeric(base$P_BA_WORK[paste(i)]) *
                                    0.5) * as.numeric(base$P_TOVER[paste(i)])
  base$P_RESERVE_WORK[paste(i)] = as.numeric(base$P_INCOME_WORK[paste(i)]) +
    as.numeric(base$P_RESERVE_WORK[paste(i - 1)]) +
    as.numeric(base$P_BA_WORK[paste(i)])
  base$P_AREV_WORK[paste(i)] = base$P_AREV_WORK[paste(i)] + base$P_INCOME_WORK[paste(i)]
  base$P_ABA_WORK[paste(i)] = base$P_BA_WORK[paste(i)] + base$P_INCOME_WORK[paste(i)]
}
