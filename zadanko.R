



#labirynt

install.packages("sets")

library("sets")
sets_options("universe", seq(from = 0, to = 40, by = 0.1))
variables <- set(
  bmi =
    fuzzy_partition(varnames =
                      c(niedow = 9.25, zdro = 21.75,
                        nadw = 27.5, otyl = 35),
                    sd = 3.0),
  a1c =
    fuzzy_partition(varnames =
                      c(nisk = 4, norm = 5.25, wys = 7),
                    
                    FUN = fuzzy_cone, radius = 5),
  rating =
    fuzzy_partition(varnames =
                      c(odm = 10, stand = 5, pref = 1),
                    FUN = fuzzy_cone, radius = 5),
  bp =
    fuzzy_partition(varnames =
                      c(norm = 0, mnadcis = 10, nadcis = 20,
                        dnadci = 30), sd = 2.5)
)
rules <-
  set(
    fuzzy_rule(bmi %is% niedow || bmi %is% otyl || a1c %is% nisk, rating %is% odm),
    fuzzy_rule(bmi %is% nadw || a1c %is% nisk || bp %is% mnadcis, rating %is% stand),
    fuzzy_rule(bmi %is% zdro && a1c %is% norm && bp %is% norm, rating %is% pref)
  )
system <- fuzzy_system(variables, rules)
print(system)
plot(system)
fi <- fuzzy_inference(system, list(bmi =29, a1c=5, bp=20))
plot(fi)
gset_defuzzify(fi,"centroid")


#Zadanie domowe 1
losuj <- function(a,b){
  s <- sample(a:b,1)
  return(s)
  }

standaryzuj <- function(v){
  i=1
  w=v
  for(i in 1:length(v))
  {
    w[i]= (v[i]-mean(v))/sd(v)
    i=i+1
  }
  return(w)
}
normalizuj <- function(v){
  i=1
  w=v
  for(i in 1:length(v))
  {
    w[i]=(v[i]-min(v))/(max(v)-min(v))
    i=i+1
  }
  return(w)
  
}

#Zadanie domowe ad2



matplot(rok,dat,main="Ludnosc w miastach Polski",xlab ="Lata" ,ylab = "Liczba ludnosci [w tys.]",type = "o", pch=1,
        col= 1:3)
legend("topleft",legend = c("Gdansk","Poznan","Szczecin"), col = 1:3, pch=1)

#Zadanie domowe 2

labirynt=matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,0,0,0,0,0,0,0,0,1,1,0,1,0,1,0,0,1,1,1,1,1,1,0,0,0,0,1,0,0,1,0, 0,1,1,1,0,1,1,1,0,0,1,1,0,1,1,0,0,0,1,0,0,1,0,1,0,1,1,0,1,1,0,0,1,1,0,0,0,1,1,0,0,0,0,0,0,0,0,1,0,1,1,1,1,0,1,1,0,1,1,0,0,1,1,0,1,0,1,0,0,0,1,1,0,1,1,0,0,0,0,0,1,0,0,0,3,1,1,1,1,1,1,1,1,1,1,1,1,1),nrow = 12, ncol = 12)
chodzenie<- function(v){
  x=2
  y=2
 
  i=1
  for (i in 1:length(v)) {
    if(v[i]=="G" && labirynt[x-1,y]!=1){
      x=x-1
    }
    if(v[i]=="P" && labirynt[x,y+1]!=1){
      
      y=y+1
    }
    if(v[i]=="L" && labirynt[x,y-1]!=1){
     
      y=y-1
    }
    if(v[i]=="D" && labirynt[x+1,y]!=1){
      x=x+1
    }
    if(labirynt[x,y]==3)
    {
      print("koniec")
      return(c(x,y))
    }
    i=i+1
  }
  return(c(x,y))
}
chodzenie(c("P","P","D","P","P","G","P","P","D","D","P","P","P","D","D","L","D","D","D","P","D","D","D"))

