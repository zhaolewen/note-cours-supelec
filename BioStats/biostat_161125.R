m1 = matrix(rnorm(100 * 10000),100,10000)
m2 = matrix(rnorm(100 * 10000),100,10000)

m = rbind(m1,m2)
dim(m)
y=rep(1:2,each=100)

p = apply(m,2,function(x){
  t.test(x~y)$p.value
})

which(p<0.05)

padj_bonf <- p*ncol(m)
sum(padj_bonf<0.05)

padj_bonf[padj_bonf>1]=1

pBH = p.adjust(p,method="BH")
pBonf = p.adjust(p, method = "bonferroni")

sum(pBH<0.05)

p = data.frame(p = p, pbonf = pBonf, pBH = pBH)
head(p, 30)