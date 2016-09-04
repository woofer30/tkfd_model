base$H_HM_EXP_PC_0_14=base$H_HM_EXP_PC_0_14[index(base)==2013]
base$H_HM_EXP_PC_15_44=base$H_HM_EXP_PC_15_44[index(base)==2013]
base$H_HM_EXP_PC_45_64=base$H_HM_EXP_PC_45_64[index(base)==2013]
base$H_HM_EXP_PC_65_74=base$H_HM_EXP_PC_65_74[index(base)==2013]
base$H_HM_EXP_PC_75_o=base$H_HM_EXP_PC_75_o[index(base)==2013]


##年齢別国民医療費（十億円）
base$H_HM_EXP_0_14=dexpd(base$H_HM_EXP_0_14,
                         base$H_HM_EXP_PC_0_14*base$M_POP_0_14*(((1+base$M_GDP_R_PC_GR)^base$H_RUR-1)*H_ELA_0_14+1)/1000
                         ,2013)
base$H_HM_EXP_15_44=dexpd(base$H_HM_EXP_15_44,
                          base$H_HM_EXP_PC_15_44*base$M_POP_15_44*(((1+base$M_GDP_R_PC_GR)^base$H_RUR-1)*H_ELA_15_44+1)/1000
                          ,2013)
base$H_HM_EXP_45_64=dexpd(base$H_HM_EXP_45_64,
                          base$H_HM_EXP_PC_45_64*base$M_POP_45_64*(((1+base$M_GDP_R_PC_GR)^base$H_RUR-1)*H_ELA_45_64+1)/1000
                          ,2013)
base$H_HM_EXP_65_74=dexpd(base$H_HM_EXP_65_74,
                          base$H_HM_EXP_PC_65_74*base$M_POP_65_74*(((1+base$M_GDP_R_PC_GR)^base$H_RUR-1)*H_ELA_65_74+1)/1000
                          ,2013)
base$H_HM_EXP_75_o=dexpd(base$H_HM_EXP_75_o,
                         base$H_HM_EXP_PC_75_o*base$M_POP_75_o*(((1+base$M_GDP_R_PC_GR)^base$H_RUR-1)*H_ELA_75_o+1)/1000
                         ,2013)


##国民医療費（十億円）
base$H_HM_EXP=base$H_HM_EXP_0_14+base$H_HM_EXP_15_44+base$H_HM_EXP_45_64+base$H_HM_EXP_65_74+base$H_HM_EXP_75_o

##調整率
base$H_HM_ad_rate=base$H_HM_EXP_SHAKAI/base$H_HM_EXP_KOKUMIN
base$H_HM_ad_rate=base$H_HM_ad_rate[index(base)==2013]

##医療分野の社会保障給付費（十億円）
base$H_HM_EXP_social=base$H_HM_EXP*base$H_HM_ad_rate

##自己負担を除いたもの（十億円）
base$H_HM_de_EXP_0_14=base$H_HM_EXP_0_14*0.7
base$H_HM_de_EXP_15_44=base$H_HM_EXP_15_44*0.7
base$H_HM_de_EXP_45_64=base$H_HM_EXP_45_64*0.7
base$H_HM_de_EXP_65_74=base$H_HM_EXP_65_74*0.7
base$H_HM_de_EXP_75_o=base$H_HM_EXP_75_o*0.9

##各保険加入者総数（千人）
base$H_HM_POP_KOUKI=base$H_HM_POP_KOUKI[index(base)==2013]
base$H_HM_POP_KOKUHO=base$H_HM_POP_KOKUHO[index(base)==2013]
base$H_HM_POP_KENPO=base$H_HM_POP_KENPO[index(base)==2013]
base$H_HM_POP_KUMIAI=base$H_HM_POP_KUMIAI[index(base)==2013]
base$H_HM_POP_KYOSAI=base$H_HM_POP_KYOSAI[index(base)==2013]
base$H_HM_POP_ins=base$H_HM_POP_KOUKI+base$H_HM_POP_KOKUHO+base$H_HM_POP_KENPO+base$H_HM_POP_KUMIAI+base$H_HM_POP_KYOSAI

##各保険加入率
base$H_HM_POP_KOUKI_rate=base$H_HM_POP_KOUKI/base$H_HM_POP_ins
base$H_HM_POP_KOKUHO_rate=base$H_HM_POP_KOKUHO/base$H_HM_POP_ins
base$H_HM_POP_KENPO_rate=base$H_HM_POP_KENPO/base$H_HM_POP_ins
base$H_HM_POP_KUMIAI_rate=base$H_HM_POP_KUMIAI/base$H_HM_POP_ins
base$H_HM_POP_KYOSAI_rate=base$H_HM_POP_KYOSAI/base$H_HM_POP_ins

##国民健康保険加入者（千人）
base$H_HM_POP_KOKUHO_0_14=base$M_POP_0_14*base$H_HM_POP_KOKUHO_rate
base$H_HM_POP_KOKUHO_15_44=base$M_POP_15_44*base$H_HM_POP_KOKUHO_rate
base$H_HM_POP_KOKUHO_45_64=base$M_POP_45_64*base$H_HM_POP_KOKUHO_rate
base$H_HM_POP_KOKUHO_65_74=base$M_POP_65_74*base$H_HM_POP_KOKUHO_rate

##協会けんぽ加入者（千人）
base$H_HM_POP_KENPO_0_14=base$M_POP_0_14*base$H_HM_POP_KENPO_rate
base$H_HM_POP_KENPO_15_44=base$M_POP_15_44*base$H_HM_POP_KENPO_rate
base$H_HM_POP_KENPO_45_64=base$M_POP_45_64*base$H_HM_POP_KENPO_rate
base$H_HM_POP_KENPO_65_74=base$M_POP_65_74*base$H_HM_POP_KENPO_rate

##組合健保加入者（千人）
base$H_HM_POP_KUMIAI_0_14=base$M_POP_0_14*base$H_HM_POP_KUMIAI_rate
base$H_HM_POP_KUMIAI_15_44=base$M_POP_15_44*base$H_HM_POP_KUMIAI_rate
base$H_HM_POP_KUMIAI_45_64=base$M_POP_45_64*base$H_HM_POP_KUMIAI_rate
base$H_HM_POP_KUMIAI_65_74=base$M_POP_65_74*base$H_HM_POP_KUMIAI_rate

##共済組合加入者（千人）
base$H_HM_POP_KYOSAI_0_14=base$M_POP_0_14*base$H_HM_POP_KYOSAI_rate
base$H_HM_POP_KYOSAI_15_44=base$M_POP_15_44*base$H_HM_POP_KYOSAI_rate
base$H_HM_POP_KYOSAI_45_64=base$M_POP_45_64*base$H_HM_POP_KYOSAI_rate
base$H_HM_POP_KYOSAI_65_74=base$M_POP_65_74*base$H_HM_POP_KYOSAI_rate

##年齢別保険加入者総数（千人）
base$H_HM_POP_ins_0_14=base$H_HM_POP_KOKUHO_0_14+base$H_HM_POP_KENPO_0_14+base$H_HM_POP_KUMIAI_0_14+base$H_HM_POP_KYOSAI_0_14
base$H_HM_POP_ins_15_44=base$H_HM_POP_KOKUHO_15_44+base$H_HM_POP_KENPO_15_44+base$H_HM_POP_KUMIAI_15_44+base$H_HM_POP_KYOSAI_15_44
base$H_HM_POP_ins_45_64=base$H_HM_POP_KOKUHO_45_64+base$H_HM_POP_KENPO_45_64+base$H_HM_POP_KUMIAI_45_64+base$H_HM_POP_KYOSAI_45_64
base$H_HM_POP_ins_65_74=base$H_HM_POP_KOKUHO_65_74+base$H_HM_POP_KENPO_65_74+base$H_HM_POP_KUMIAI_65_74+base$H_HM_POP_KYOSAI_65_74

##後期高齢者医療保険料（十億円）（都道府県・市町村の負担も公費負担としている）
base$H_HM_EXP_KOUKI_pub=base$H_HM_de_EXP_75_o*0.5
base$H_HM_EXP_KOUKI_pre=base$H_HM_de_EXP_75_o*0.5

##国民健康保険料（十億円）(都道府県の負担も公費負担としている)
base$H_HM_EXP_KOKUHO_0_14=base$H_HM_POP_KOKUHO_0_14/base$H_HM_POP_ins_0_14*base$H_HM_de_EXP_0_14
base$H_HM_EXP_KOKUHO_15_44=base$H_HM_POP_KOKUHO_15_44/base$H_HM_POP_ins_15_44*base$H_HM_de_EXP_15_44
base$H_HM_EXP_KOKUHO_45_64=base$H_HM_POP_KOKUHO_45_64/base$H_HM_POP_ins_45_64*base$H_HM_de_EXP_45_64
base$H_HM_EXP_KOKUHO_65_74=base$H_HM_POP_KOKUHO_65_74/base$H_HM_POP_ins_65_74*base$H_HM_de_EXP_65_74
base$H_HM_EXP_KOKUHO=base$H_HM_EXP_KOKUHO_0_14+base$H_HM_EXP_KOKUHO_15_44+base$H_HM_EXP_KOKUHO_45_64+base$H_HM_EXP_KOKUHO_65_74
base$H_HM_EXP_KOKUHO_pub=base$H_HM_EXP_KOKUHO*0.5
base$H_HM_EXP_KOKUHO_pre=base$H_HM_EXP_KOKUHO*0.5

##協会けんぽ保険料（十億円）
base$H_HM_EXP_KENPO_0_14=base$H_HM_POP_KENPO_0_14/base$H_HM_POP_ins_0_14*base$H_HM_de_EXP_0_14
base$H_HM_EXP_KENPO_15_44=base$H_HM_POP_KENPO_15_44/base$H_HM_POP_ins_15_44*base$H_HM_de_EXP_15_44
base$H_HM_EXP_KENPO_45_64=base$H_HM_POP_KENPO_45_64/base$H_HM_POP_ins_45_64*base$H_HM_de_EXP_45_64
base$H_HM_EXP_KENPO_65_74=base$H_HM_POP_KENPO_65_74/base$H_HM_POP_ins_65_74*base$H_HM_de_EXP_65_74
base$H_HM_EXP_KENPO=base$H_HM_EXP_KENPO_0_14+base$H_HM_EXP_KENPO_15_44+base$H_HM_EXP_KENPO_45_64+base$H_HM_EXP_KENPO_65_74
base$H_HM_EXP_KENPO_pub=base$H_HM_EXP_KENPO*0.164
base$H_HM_EXP_KENPO_pre=base$H_HM_EXP_KENPO*0.836

##組合健保保険料（十億円）
base$H_HM_EXP_KUMIAI_0_14=base$H_HM_POP_KUMIAI_0_14/base$H_HM_POP_ins_0_14*base$H_HM_de_EXP_0_14
base$H_HM_EXP_KUMIAI_15_44=base$H_HM_POP_KUMIAI_15_44/base$H_HM_POP_ins_15_44*base$H_HM_de_EXP_15_44
base$H_HM_EXP_KUMIAI_45_64=base$H_HM_POP_KUMIAI_45_64/base$H_HM_POP_ins_45_64*base$H_HM_de_EXP_45_64
base$H_HM_EXP_KUMIAI_65_74=base$H_HM_POP_KUMIAI_65_74/base$H_HM_POP_ins_65_74*base$H_HM_de_EXP_65_74
base$H_HM_EXP_KUMIAI=base$H_HM_EXP_KUMIAI_0_14+base$H_HM_EXP_KUMIAI_15_44+base$H_HM_EXP_KUMIAI_45_64+base$H_HM_EXP_KUMIAI_65_74
base$H_HM_EXP_KUMIAI_fix=base$H_HM_EXP_KUMIAI_fix[index(base)==2013]
base$H_HM_EXP_KUMIAI_pub=base$H_HM_EXP_KUMIAI_fix
base$H_HM_EXP_KUMIAI_pre=base$H_HM_EXP_KUMIAI-base$H_HM_EXP_KUMIAI_fix

##共済保険保険料（十億円）
base$H_HM_EXP_KYOSAI_0_14=base$H_HM_POP_KYOSAI_0_14/base$H_HM_POP_ins_0_14*base$H_HM_de_EXP_0_14
base$H_HM_EXP_KYOSAI_15_44=base$H_HM_POP_KYOSAI_15_44/base$H_HM_POP_ins_15_44*base$H_HM_de_EXP_15_44
base$H_HM_EXP_KYOSAI_45_64=base$H_HM_POP_KYOSAI_45_64/base$H_HM_POP_ins_45_64*base$H_HM_de_EXP_45_64
base$H_HM_EXP_KYOSAI_65_74=base$H_HM_POP_KYOSAI_65_74/base$H_HM_POP_ins_65_74*base$H_HM_de_EXP_65_74
base$H_HM_EXP_KYOSAI=base$H_HM_EXP_KYOSAI_0_14+base$H_HM_EXP_KYOSAI_15_44+base$H_HM_EXP_KYOSAI_45_64+base$H_HM_EXP_KYOSAI_65_74
base$H_HM_EXP_KYOSAI_pub=base$H_HM_EXP_KYOSAI
base$H_HM_EXP_KYOSAI_pre=0


##医療保険料対名目GDP比
base$H_HM_EXP_pre_per_GDP=base$H_HM_EXP_pre/base$M_GDP
base$H_HM_EXP_pre_per_GDP=base$H_HM_EXP_pre_per_GDP[index(base)==2013]

##名目GDPから算出される患者の自己負担
base$H_HM_EXP_pat=base$H_HM_pat_rates*base$H_HM_EXP

##名目GDPから算出される自己負担を除く医療費
base$H_HM_de_EXP=base$H_HM_EXP-base$H_HM_EXP_pat

##名目GDPから算出される医療保険料負担
base$H_HM_EXP_pre_GDP=base$M_GDP*base$H_HM_EXP_pre_per_GDP

##医療保険における公費負担（十億円）
base$H_HM_EXP_pub=base$H_HM_de_EXP-base$H_HM_EXP_pre_GDP


##介護認定者出現率
base$H_HC_CER_Rate_40_64=base$H_HC_CER_Rate_40_64[index(base)==2013]
base$H_HC_CER_Rate_65_74=base$H_HC_CER_Rate_65_74[index(base)==2013]
base$H_HC_CER_Rate_75_o=base$H_HC_CER_Rate_75_o[index(base)==2013]

##介護認定者数（千人）
base$H_HC_POP_40_64=dexpd(base$H_HC_POP_40_64,
                          base$M_POP_40_64*base$H_HC_CER_Rate_40_64,2013)
base$H_HC_POP_65_74=dexpd(base$H_HC_POP_65_74,
                          base$M_POP_65_74*base$H_HC_CER_Rate_65_74,2013)
base$H_HC_POP_75_o=dexpd(base$H_HC_POP_75_o,
                         base$M_POP_75_o*base$H_HC_CER_Rate_75_o,2013)
base$H_HC_POP=base$H_HC_POP_40_64+base$H_HC_POP_65_74+base$H_HC_POP_75_o

##介護認定者に対する受給者数割合
base$H_HC_BEN_Rate=base$H_HC_BEN_Rate[index(base)==2013]

##介護受給者数（千人）
base$H_HC_POP_D=base$H_HC_POP*base$H_HC_BEN_Rate

##各介護サービス利用率
base$H_HC_SHISETU_Rate=base$H_HC_SHISETU_Rate[index(base)==2013]
base$H_HC_KYOTAKU_Rate=base$H_HC_KYOTAKU_Rate[index(base)==2013]
base$H_HC_TIIKI_Rate=base$H_HC_TIIKI_Rate[index(base)==2013]

##各サービス受給者数（千人）
base$H_HC_POP_D_SHISETU=base$H_HC_POP_D*base$H_HC_SHISETU_Rate
base$H_HC_POP_D_KYOTAKU=base$H_HC_POP_D*base$H_HC_KYOTAKU_Rate
base$H_HC_POP_D_TIIKI=base$H_HC_POP_D*base$H_HC_TIIKI_Rate

##一人当たり各介護サービス費用（2012年の値を用いている）
base$H_HC_EXP_SHISETU_PC=base$H_HC_EXP_SHISETU_PC[index(base)==2013]
base$H_HC_EXP_KYOTAKU_PC=base$H_HC_EXP_KYOTAKU_PC[index(base)==2013]
base$H_HC_EXP_TIIKI_PC=base$H_HC_EXP_TIIKI_PC[index(base)==2013]

##各介護サービス別弾性値
base$H_HC_ELA_SHISETU=base$H_HC_ELA_SHISETU[index(base)==2013]
base$H_HC_ELA_KYOTAKU=base$H_HC_ELA_KYOTAKU[index(base)==2013]
base$H_HC_ELA_TIIKI=base$H_HC_ELA_TIIKI[index(base)==2013]

##各介護サービスの給付費
base$H_HC_EXP_SHISETU=dexpd(base$H_HC_EXP_SHISETU,
                            base$H_HC_POP_D_SHISETU*base$H_HC_EXP_SHISETU_PC*(((1+base$M_GDP_R_PC_GR)^base$H_RUR-1)*base$H_HC_ELA_SHISETU+1),2013)
base$H_HC_EXP_KYOTAKU=dexpd(base$H_HC_EXP_KYOTAKU,
                            base$H_HC_POP_D_KYOTAKU*base$H_HC_EXP_KYOTAKU_PC*(((1+base$M_GDP_R_PC_GR)^base$H_RUR-1)*base$H_HC_ELA_KYOTAKU+1),2013)
base$H_HC_EXP_TIIKI=dexpd(base$H_HC_EXP_TIIKI,
                          base$H_HC_POP_D_TIIKI*base$H_HC_EXP_TIIKI_PC*(((1+base$M_GDP_R_PC_GR)^base$H_RUR-1)*base$H_HC_ELA_TIIKI+1),2013)

##介護総費用（十億円）
base$H_HC_EXP=base$H_HC_EXP_SHISETU+base$H_HC_EXP_KYOTAKU+base$H_HC_EXP_TIIKI

##介護総給付費（十億円）
base$H_HC_de_EXP=base$H_HC_EXP*0.9

##負担額（十億円）
base$H_HC_EXP_pub=base$H_HC_de_EXP*0.5
base$H_HC_EXP_pre=base$H_HC_de_EXP*0.5