#Pre -processing data
install.packages('readxl')
library(readxl)
da_1 <- read_excel("C://Users//ZXH//Desktop//ecoli_core_model.xls",sheet =1)
da_1[,1]
dim(da_1)[2]
colnames(da_1)
rownames(da_1)
size = dim(da_1)
size_row = size[1]
size_col = size[2]-1

row_names = da_1[,1]
col_names = as.data.frame(colnames(da_1[1:size_col+1])) 

H_Tail = matrix(data = 0,nrow = size_row,ncol = size_col,byrow = TRUE,dimnames = list(t(row_names),t(col_names)))
H_Head = matrix(data = 0,nrow = size_row,ncol = size_col,byrow = TRUE,dimnames = list(t(row_names),t(col_names)))

for(i in 1:size_row){
  for(j in 1:size_col){
    if(da_1[i,j+1]<0){
      H_Tail[i,j] = 1
    }
    if(da_1[i,j+1]>0){
      H_Head[i,j] = 1
    }
  }
} 
DELETE_1 = 0
DELETE_2 = 0
DELETE_ROW_1 = c()
DELETE_ROW_2 = c()
for(i in 1:size_row){
  if(sum(H_Tail[i,])==0){
    DELETE_1 = DELETE_1+1
    DELETE_ROW_1[DELETE_1] = -i
  }
  if(sum(H_Head[i,])==0){
    DELETE_2 = DELETE_2+1
    DELETE_ROW_2[DELETE_2] = -i
  }
}
DELETE_ROW = union(DELETE_ROW_1,DELETE_ROW_2)
DELETE_3 = 0
DELETE_4 = 0
DELETE_COL_1 = c()
DELETE_COL_2 = c()
for(i in 1:size_col){
  if(sum(H_Tail[,i])==0){
    DELETE_3 = DELETE_3+1
    DELETE_COL_1[DELETE_3] = -i
  }
  if(sum(H_Head[,i])==0){
    DELETE_4 = DELETE_4+1
    DELETE_COL_2[DELETE_4] = -i
  }
}
DELETE_COL = union(DELETE_COL_1,DELETE_COL_2)
H_Tail <-H_Tail[DELETE_ROW,]
H_Head <-H_Head[DELETE_ROW,]
H_Tail <-H_Tail[,DELETE_COL]
H_Head <-H_Head[,DELETE_COL]

D_Tail_v = matrix(data=0,nrow=dim(H_Tail)[1],ncol=dim(H_Tail)[1],byrow = TRUE)
D_Head_v = matrix(data=0,nrow=dim(H_Head)[1],ncol=dim(H_Head)[1],byrow = TRUE)
for(k in 1:dim(H_Tail)[1]){
  D_Tail_v[k,k] = sum(H_Tail[k,])
  D_Head_v[k,k] = sum(H_Head[k,])
}
D_Tail_e = matrix(data=0,nrow=dim(H_Tail)[2],ncol=dim(H_Tail)[2],byrow = TRUE)
D_Head_e = matrix(data=0,nrow=dim(H_Head)[2],ncol=dim(H_Head)[2],byrow = TRUE)
for(l in 1:dim(H_Tail)[2]){
  D_Tail_e[l,l] = sum(H_Tail[,l])
  D_Head_e[l,l] = sum(H_Head[,l])
}

#Calculate the matrix
P = (solve(D_Tail_v))%*%H_Tail%*%(solve(D_Head_e))%*%(t(H_Head))
#验证
for(a in 1:50){
  print(sum(P[a,]))
}
Pagerank_powermethod = function(M, k){
  #使用幂法求矩阵的特征向量
  # M: 给定的矩阵  k:迭代次数
  m = dim(M)[1]
  v = matrix(data=0.08805,nrow=1,ncol=m,byrow = TRUE)
  for(i in 1:k){
    v = v%*%P
  }
  return(v)
}
PR = Pagerank_powermethod(P,150)
PR = t(PR)
PR[order(PR[,1],decreasing = T),][1:10]
