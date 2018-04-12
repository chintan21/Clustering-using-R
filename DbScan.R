
dbclustering<-function(x,eps,m){
	dist.matrix<-as.matrix(dist(x,method="euclidean", diag = FALSE, upper = FALSE, p=2))

	clus.vector<-vector(mode="numeric",length=nrow(x))
	status<-rep("un",nrow(x))
	core.vector<-rep("",nrow(x))
	for(i in 1:nrow(x))
	{
		nb<-find.nbr(x,dist.matrix,i,eps)
		if(nrow(nb)>=m){
			core.vector[i]="core"

		}
	}
	cc<-0
	for(i in 1:nrow(x)){
		if(status[i]=="un"){
			if(clus.vector[i]==0){
				nb<-find.nbr(x,dist.matrix,i,eps)

				if(nrow(nb)>=m){
					status[i]<-"visited"
					cc<-cc+1
					clus.vector[i]<-cc
					clus.vector[nb$nr]<-cc
					jj<-c()
					jj<-union(jj,c(find.corenbr(x,dist.matrix,i,eps,core.vector)$nr))
					j<-1
					repeat{
						if(status[jj[j]]=="un" ){
							nbr<-find.nbr(x,dist.matrix,jj[j],eps)
							if(nrow(nbr)>=m){
								clus.vector[nbr$nr]<-cc
								status[jj[j]]<-"visited"
								corenbr<-as.data.frame(c(find.corenbr(x,dist.matrix,jj[j],eps,core.vector)$nr))
								corecore<-c()
								for(q in 1:nrow(corenbr)){
									if(status[corenbr[q,1]]=="un"){
										corecore<-union(corecore,c(corenbr[q,1]))
									}
								}
								jj<-union(jj,corecore)
								remove(corecore)
								j<-1
							}
						}
						else{
							j<-j+1
						}

						if(is.element(FALSE,status[jj]=="visited")){
							}else{
								break
							}

						} 
					}
					else{

						status[i]<-"noise"
					}

				}
				else{
					status[i]<-"visited"
					b<-clus.vector[i]
					if(core.vector[i]=="core"){
						clus.vector[nb$nr]<-b
					}

				}
			}
		}

		list2<-list(table(clus.vector),clus.vector)
		names(list2)<-c("cluster","clustervector")
		return(list2)
	}

	find.nbr<-function(x,distmatrix,i,eps){
		nr<-c()
		nb<-data.frame()
		for(k in 1:ncol(distmatrix))
		{
			if(distmatrix[i,k]<=eps ){
				nb<-rbind(nb,x[k,])
				nr<-c(nr,k)
			}
		}
		nb<-cbind(nb,nr)
		return(nb)
	}


	find.corenbr<-function(x,distmatrix,i,eps,core.vect){
		nr<-c()
		nb<-data.frame()
		for(k in 1:ncol(distmatrix))
		{
			if(distmatrix[i,k]<=eps && distmatrix[i,k]!=0 && core.vect[k]=="core"){
				nb<-rbind(nb,x[k,])
				nr<-c(nr,k)
			}
		}
		nb<-cbind(nb,nr)
		return(nb)
	}
