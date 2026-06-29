correlation.analysis <- function(x, names1, names2){
  nr = length(names1) # nubmer of variables in subset1
  nc = length(names2) # number of variables in subset2
    
  P = matrix(0, nrow = nr, ncol = nc) # Pearson correlation matrix
  rownames(P) = names1 # names of variables in subset1
  colnames(P) = names2 # names of variables in subset1
    
  P.test = P
  P.test.adj = P
  S = P
  S.test = P
  S.test.adj = P
    
  for(i in 1:nr) {
    for(j in 1:nc) {
      P[i,j] = cor(x[names1[i]][,1],
                   x[names2[j]][,1],
                   method = "pearson")
      S[i,j] = cor(x[names1[i]][,1],
                   x[names2[j]][,1],
                   method = "spearman")
      t1 = cor.test(x[names1[i]][,1],
                    x[names2[j]][,1],
                    method = "pearson")
      t2 = cor.test(x[names1[i]][,1],
                    x[names2[j]][,1],
                    method = "spearman")
      P.test[i,j] = t1$p.value
      S.test[i,j] = t2$p.value}
    P.test.adj[i, ] = p.adjust(P.test[i, ], method = "BH")
    S.test.adj[i, ] = p.adjust(S.test[i, ], method = "BH")}
  
  return(list(P = P, P.test = P.test, P.test.adj = P.test.adj, S = S, S.test = S.test,
              S.test.adj = S.test.adj))}
    
#Audio.all = correlation.analysis(D1, MRI.names, Audio.names)