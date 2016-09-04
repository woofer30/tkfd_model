## すべてのデータをクリア
rm(list = ls(all = TRUE))

## **********************************************************************
## プロトタイプモデル 基幹部門 初期設定
## **********************************************************************


## ディレクトリ設定 (プログラム内で設定可能)
## setwd('D:\\base')

## options(digits=10)

## 時系列データ処理ライブラリ
library("zoo")


## **********************************************************************
## プロトタイプモデル 基幹部門 データ読み込み分野
## **********************************************************************

## csvデータの読込
base = read.zoo("data/data_m.csv",  FUN=identity, index = "year",  sep = ",", header=TRUE, na.strings = c("NA",""))
fis  = read.zoo("data/data_f.csv",  FUN=identity, index = "year",  sep = ",", header=TRUE, na.strings = c("NA",""))
hmc  = read.zoo("data/data_h.csv",  FUN=identity, index = "year",  sep = ",", header=TRUE, na.strings = c("NA",""))
pen  = read.zoo("data/data_p.csv",  FUN=identity, index = "year",  sep = ",", header=TRUE, na.strings = c("NA",""))


## データの統合前の形式チェック
is.numeric(base)
is.numeric(fis)
is.numeric(hmc)
is.numeric(pen)


## データの統合
base=merge(base,fis)
is.numeric(base)
rm(fis)
base=merge(base,hmc)
is.numeric(base)
rm(hmc)
base=merge(base,pen)
is.numeric(base)
rm(pen)


# 西暦を1行目に入れる
bname = colnames(base)
base = cbind(zoo(index(base), order.by = index(base) ) ,base)
colnames(base) <- c("yeard", bname)

# 予約変数 全て1の変数
base$one=1
base$zero=0*base$one
base$nas=NA*base$one

## **********************************************************************
## プロトタイプモデル 基幹部門 関数作成分野
## **********************************************************************

source("src/func.r", encoding="UTF-8")

## **********************************************************************
## プロトタイプモデル 基幹部門 各部門 仮想値（政策変更には以下の仮想値を変更してください）
## **********************************************************************

source("src/policy.r", encoding="UTF-8")


M_CPI_GR_A	 =  M_CPI_GR_A_D/100		# 消費者物価変化率 	百分の一
M_TFP_GR_A	 =  M_TFP_GR_A_D/100		# TFP上昇率 		百分の一
M_BUNPAI	 =  M_BUNPAI_D/100		# 資本分配率 		百分の一
M_POP_GR_A	 =  M_POP_GR_A_D/100		# 人口成長率 		百分の一
F_BOND_SPREAD 	 =  F_BOND_SPREAD_D/100		# 国債利子率スプレッド	百分の一
H_HM_pat_rate 	 =  H_HM_pat_rate_D*0.1175137	# 名目GDPから算出される患者の医療費自己負担率
H_ELA_0_14	 =  1.000			# 年齢別医療弾性値
H_ELA_15_44	 =  1.000			# 年齢別医療弾性値
H_ELA_45_64	 =  1.000			# 年齢別医療弾性値
H_ELA_65_74	 =  1.000			# 年齢別医療弾性値
H_ELA_75_o	 =  1.000			# 年齢別医療弾性値


F_DLT_P1 	 =  F_DLT_P1_D/100*(1/(1-1/5.4))	# 消費税率再引上げ率 百分の一(軽減税率なし)
F_DLT_P0 	 =  F_DLT_P0_D/100			# 消費税率再引上げ率 百分の一(2017年引き上げ予定分)
F_DLT_P 	 =  F_DLT_P_D/100			# 消費税率再引上げ率 百分の一


# 桁修正
source("src/digit_m.r", encoding="UTF-8", echo=TRUE) 
source("src/digit_h.r", encoding="UTF-8", echo=TRUE) 
source("src/digit_p.r", encoding="UTF-8", echo=TRUE) 
source("src/digit_f.r", encoding="UTF-8", echo=TRUE) 

## **********************************************************************
## プロトタイプモデル 将来予測部門 標準計算
## **********************************************************************


## 
## マクロ分野（他ファイル参照）
## 


source("src/prj_m.r", encoding="UTF-8", echo=TRUE) 

##base$M_GDP=base$M_GDP


## 
## 年金分野（他ファイル参照：未作成）
## 
source("src/prj_p.r", encoding="UTF-8", echo=TRUE) # 社会保障

## 
## 医療介護子供分野（他ファイル参照：未作成）
## 
source("src/prj_h.r", encoding="UTF-8", echo=TRUE) # 社会保障

## 
## 財政分野（他ファイル参照）
## 
source("src/prj_f.r", encoding="UTF-8", echo=TRUE) # 財政


## **********************************************************************
## プロトタイプモデル 将来予測部門 繰り返し計算（ガウス・ザイデル法）
## **********************************************************************

nitr=0
dif=1
while(dif > 0.01){
			base$F_PRB_b=base$F_PRB
			source("src/prj_p.r", encoding="UTF-8")
			source("src/prj_h.r", encoding="UTF-8")
			source("src/prj_f.r", encoding="UTF-8")
			difv=abs(base$F_PRB_b-base$F_PRB)
			dif=max(difv, na.rm=TRUE)
			nitr=nitr+1
		}

nitr
dif




## **********************************************************************
## プロトタイプモデル 基幹部門 アウトプット分野
## **********************************************************************

source("src/fig.r", encoding="UTF-8", echo=TRUE)


rm(list=ls(all=TRUE))

