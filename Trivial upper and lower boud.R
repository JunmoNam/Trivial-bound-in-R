#==========================================================================
# Topic : Social Network Theory
#                 Trivial Upper and Lower bound computation using actor relation data
# Date : 2019. 03. 31
# Author : Junmo Nam
#==========================================================================


#==========================================================================
# Load data and packages 
#==========================================================================

pkg = c('dplyr','igraph','foreach','doParallel')
sapply(pkg,require,character.only = T)

#read csv file and build Graph object using igraph functions
mdf = read.csv('actor matrix(all, Cal, 658).csv')
G = graph_from_data_frame(mdf,directed = F)
nodes = V(G)$name
diameter(G)



#==========================================================================
# Build trivial bounder : diameter
#==========================================================================

grid = 2:100

cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

grid_res = foreach(k = grid,.packages = pkg,.combine = rbind) %dopar% {
  set.seed(1004)
  rnd = split(nodes, ceiling(seq_along(nodes)/k))
  #set start bound
  rndf = mdf[which(mdf$Var1 %in% rnd[[1]] | mdf$Var2 %in% rnd[[1]]),]
  rg = graph_from_data_frame(rndf,directed=F)
  res.up = 2*diameter(rg)
  res.down = diameter(rg)
  #do iter 
  for(i in 2:length(rnd)){
    rndf = mdf[which(mdf$Var1 %in% rnd[[i]] | mdf$Var2 %in% rnd[[i]]),]
    rg = graph_from_data_frame(rndf,directed=F)
    d = diameter(rg)
    if(abs(res.down[i-1] - res.up[i-1])>1){ #limit = differ is smaller than 1
      res.down = append(res.down,ifelse(d >res.down[i-1],d,res.down[i-1])) #update when d is bigger than privious upper bound
      res.up = append(res.up,ifelse(2*d<res.up[i-1],2*d,res.up[i-1])) #update when d is smaller than privious upper bound
    }
    else if(res.down[i-1] > res.up[i-1]){
      paste0('low bound exceed upper boud, stop at ',i) %>% print
      res.down[i] = res.down[i-1]
      res.up[i] = res.up[i-1]
      break
    }
    else{
      paste0('find expected result by ',i) %>% print
      break
    }
    
  }
  data.frame(down = res.down[length(res.down)],up = res.up[length(res.up)],iter = i,comp_num = k,comp = length(rnd)) %>% return()
  
}

# absolute difference between actual diameter and trivial bound
differ = abs(grid_res[,1:2] %>% rowMeans - diameter(G)) 

# sort grid search result by diffrence
grid_res[order(differ,decreasing = F),] %>% head
grid_res[which.min(differ),]
100*grid_res[which.min(differ),4]/length(nodes)

# add column to grid result dataframe
grid_res$resd = differ

# penalized mean rank 
#           assume residual rank is more important (7:3)
grid_res[rowMeans(data.frame(resd = rank(differ)*0.7,
                             complex = 0.3*rank(grid_res$iter/grid_res$comp_num))) %>%
           order(decreasing = F),] %>% head(5) #resorted dataframe


# repeat minimum class
rnd = split(nodes, ceiling(seq_along(nodes)/32))

#set start bound
rndf = mdf[which(mdf$Var1 %in% rnd[[1]] | mdf$Var2 %in% rnd[[1]]),]
rg = graph_from_data_frame(rndf,directed=F)
res.up = 2*diameter(rg) # upper bound = 2 * Graph diameter
res.down = diameter(rg) # lower bound = Graph diameter

# do iteration
for(i in 2:length(rnd)){
  rndf = mdf[which(mdf$Var1 %in% rnd[[i]] | mdf$Var2 %in% rnd[[i]]),]
  rg = graph_from_data_frame(rndf,directed=F)
  d = diameter(rg)
  if(abs(res.down[i-1] - res.up[i-1])>1){ #limit = differ is smaller than 1
    res.down = append(res.down,ifelse(d >res.down[i-1],d,res.down[i-1])) #update when d is bigger than privious upper bound
    res.up = append(res.up,ifelse(2*d<res.up[i-1],2*d,res.up[i-1])) #update when d is smaller than privious upper bound
  }
  else if(res.down[i-1] > res.up[i-1]){
    paste0('low bound exceed upper boud, stop at ',i) %>% print
    res.down[i] = res.down[i-1]
    res.up[i] = res.up[i-1]
    break
  }
  else{
    paste0('find expected result by ',i) %>% print
    break
  }
  
}


# plot result : red = upper bound | blue = lower bound
par(mar = c(5.1, 5.1, 4.1, 2.1))
c(rep((c(res.down,res.up) %>% range())[1],i-1),(c(res.down,res.up) %>% range())[2]) %>%
  plot(type = 'n',xlab = 'Iteration',ylab = 'Est.Diameter',main="Upper & Lower Bound of Diameter by Iteratoin",
       cex.main =2, cex.lab=1.4, cex.axis=1.5)
lines(res.down,col = 'blue')
lines(res.up,col = 'red')





#==========================================================================
# Build trivial bounder : Radius
#==========================================================================



grid_res2 = foreach(k = grid,.packages = pkg,.combine = rbind) %dopar% {
  set.seed(1004)
  rnd = split(nodes, ceiling(seq_along(nodes)/k))
  #set start bound
  rndf = mdf[which(mdf$Var1 %in% rnd[[1]] | mdf$Var2 %in% rnd[[1]]),]
  rg = graph_from_data_frame(rndf,directed=F)
  res.up = 2*radius(rg)
  res.down = radius(rg)
  #do iter 
  for(i in 2:length(rnd)){
    rndf = mdf[which(mdf$Var1 %in% rnd[[i]] | mdf$Var2 %in% rnd[[i]]),]
    rg = graph_from_data_frame(rndf,directed=F)
    d = radius(rg)
    if(abs(res.down[i-1] - res.up[i-1])>1){ #limit = differ is smaller than 1
      res.down = append(res.down,ifelse(d >res.down[i-1],d,res.down[i-1])) #update when d is bigger than privious upper bound
      res.up = append(res.up,ifelse(2*d<res.up[i-1],2*d,res.up[i-1])) #update when d is smaller than privious upper bound
    }
    else if(res.down[i-1] > res.up[i-1]){
      paste0('low bound exceed upper boud, stop at ',i) %>% print
      res.down[i] = res.down[i-1]
      res.up[i] = res.up[i-1]
      break
    }
    else{
      paste0('find expected result by ',i) %>% print
      break
    }
    
  }
  data.frame(down = res.down[length(res.down)],up = res.up[length(res.up)],iter = i,comp_num = k,
             comp = length(rnd)) %>% return()
  
}

stopCluster(cl) #stop cluster


#residual
differ = abs(grid_res2[,1:2] %>% rowMeans()-radius(G))
grid_res2[order(differ,decreasing = F),] %>% head
grid_res2[which.min(differ),]
100*grid_res2[which.min(differ),4]/length(nodes)

grid_res2$resd = differ

#penalized mean rank - assume residual rank is more important
grid_res2[rowMeans(data.frame(resd = rank(differ)*0.7,
                              complex = 0.3*rank(grid_res2$iter/grid_res2$comp_num))) %>%
            order(decreasing = F),] %>% head(5)


##########################################################################################

#repeat minimum class
set.seed(1004)
rnd = split(nodes, ceiling(seq_along(nodes)/83))
#set start bound
rndf = mdf[which(mdf$Var1 %in% rnd[[1]] | mdf$Var2 %in% rnd[[1]]),]
rg = graph_from_data_frame(rndf,directed=F)
res.up = 2*radius(rg)
res.down = radius(rg)
#do iter 
for(i in 2:length(rnd)){
  rndf = mdf[which(mdf$Var1 %in% rnd[[i]] | mdf$Var2 %in% rnd[[i]]),]
  rg = graph_from_data_frame(rndf,directed=F)
  d = radius(rg)
  if(abs(res.down[i-1] - res.up[i-1])>1){ #limit = differ is smaller than 1
    res.down = append(res.down,ifelse(d >res.down[i-1],d,res.down[i-1])) #update when d is bigger than privious upper bound
    res.up = append(res.up,ifelse(2*d<res.up[i-1],2*d,res.up[i-1])) #update when d is smaller than privious upper bound
  }
  else if(res.down[i-1] > res.up[i-1]){
    paste0('low bound exceed upper boud, stop at ',i) %>% print
    res.down[i] = res.down[i-1]
    res.up[i] = res.up[i-1]
    break
  }
  else{
    paste0('find expected result by ',i) %>% print
    break
  }
  
}
#plot res
par(mar = c(5.1, 5.1, 4.1, 2.1))
c(rep((c(res.down,res.up) %>% range())[1],i-1),(c(res.down,res.up) %>% range())[2]) %>%
  plot(type = 'n',xlab = 'Iteration',ylab = 'Est.Radius',main="Upper & Lower Bound of Radius by Iteratoin",
       cex.main =2, cex.lab=1.4, cex.axis=1.5)
lines(res.down,col = 'blue')
lines(res.up,col = 'red')

