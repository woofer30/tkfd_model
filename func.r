
## プログラム内 個別関数作成

## 将来データの作成用
## z時点までのデータはxのままに、以降の値をyのデータで埋める
## dexpd(原系列,計算式,開始年)
dexpd = function(x,y,z) {   
	ifelse(index(x)<=z,x,y)
            }

## 将来データの作成用
## z時点までのデータはxのままに、
## 以降の値を「欠損値」であればyのデータで、あればxのままで埋める
## dexpd2(原系列,計算式,開始年)
dexpd2 = function(x,y,z) {   
	ifelse(index(x)<=z,x,ifelse(is.na(x),y,x))
            }



## 基本的に蓄積関数用の関数
## データがない場合にはxのままに、あれば次式で置換
## rdexpd(判定系列,原系列,代替変数)
rdexpd = function(x,y,z) {   
	ifelse(is.na(x),y,z)
            }

## 蓄積関数用の関数 変数vを変数wを使いながら蓄積、
## ただし、変数vは変数x倍の伸縮を行う
## 金利等であればxは1より大きい系列、
## 資本減耗等であればxは1より小さい系列
## なお、引数v,w,xはベクトルであること、
## 定数を利用の際にはbase$oneをかけて使うこと
## また、蓄積変数vに初期値がない場合にはNA、
## 見つかり次第、元のデータで置きかえ、その後蓄積
## 
## acml(原系列,加算系列,比率系列,開始年)
## 

acml = function(v,w,x,y) { 
		z=max(index(v))
		for(n in y:z){
			t=n-1
			p= if(is.na(v[index(v)==t]))
			NA
			else { as.numeric(w[index(w)==n])}
				q= if(is.na(v[index(v)==t]))
				 as.numeric(v[index(v)==n])
				  else {as.numeric(v[index(v)==t])}
				r= if(is.na(x[index(x)==t]))
				 NA
				  else {as.numeric(x[index(x)==n])}
			v[index(v)==n]=p+q*r
		}
		v
		}



acml2 = function(v,w,x) { 
		y=min(index(v))
		z=max(index(v))-min(index(v))
		for(n in 1:z){
			u=y+n
			t=u-1
			p= if(is.na(v[index(v)==t]))
			0
			else { as.numeric(w[index(w)==u])}
			q= if(is.na(v[index(v)==t]))
			 as.numeric(v[index(v)==u])
			  else {as.numeric(v[index(v)==t]*x[index(x)==t])}
			v[index(v)==u]=p+q
		}
		v
		}



## 単純に1期ラグを取る場合
## lg1(原系列)

lg1 = function(v) { 
		lag(v,k=-1)
	}

## 単純に1期ラグを取る場合(ループで実装)
## lg2(原系列)

lg2 = function(v) { 
		x = min(index(v))+1
		y = max(index(v))
		z = v
		z[index(z)==x-1] = NA
		for(n in x:y){
		t=n-1
		z[index(v)==n] =
			if(is.na(v[index(v)==t]))
			NA
			else {as.numeric(v[index(v)==t])}
		n= n+1
		}
		z
	}


## 線形補間 変数vを始期と終期の間で線形連結
## intrpl = function(v,x,y)
## v：変数、x：始期、y：終期

intrpl = function(v,x,y) { 
		z=y-x
		w=as.numeric((v[index(v)==y])-as.numeric(v[index(v)==x]))/z
		a=x+1
		b=y-1
		for(n in a:b){
		c=n-1
		v[index(v)==n]=as.numeric(v[index(v)==c])+w
		}
		v
		}


