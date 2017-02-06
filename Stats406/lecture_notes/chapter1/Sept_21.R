##Odd/Even sums

A = 0; B = 0;
for (k in 1:100){
	if (k%%2 == 1){#k is odd
		A = A + k;
	} else {
		B = B + k;
	}
}

f = function(x,y){
	return(x/(1+y^2))
}

##Random generator
randn = function(){
	S = Inf
	while( S> 1){
		U1 = runif(1,-1,1);
		U2 = runif(1,-1,1);
		S = U1^2 +U2^2;
	}
	Z = sqrt(-2*log(S)/S);
	return(Z*U1);
}

lotery = function(n){
	nb_trial = 1;
	win_nb = sample(x=n,size = 3,replace=F);
	my_nb = sample(x=n,size=3,replace=F);
	while( any(win_nb != my_nb)==T){
		win_nb = sample(x=n,size = 3,replace=F);
		my_nb = sample(x=n,size=3,replace=F);
		nb_trial = nb_trial + 1;
	}
	return(nb_trial)
}

##############
##Reading files

dt1 = read.delim(file='UNData.tab',header=T,quote="")

dt2 = read.delim(file = 'spectrum.csv',header=F,skip =26)
names(dt2) =c('wavelen','reflec')

dt3 = read.csv(file = 'spectrum.csv',header=F,skip=26)
dt4 = read.csv(file = 'spectrum.csv',header=F,skip=26,sep='\t')









