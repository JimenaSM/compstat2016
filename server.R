
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# 
#http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(plyr)
library(faraway)
library(MASS)
library(sm)

shinyServer(function(input, output) {
  
  
  fun1 <- reactive({
    texto <- paste("aux <- ", input$expresion1)
    eval(parse(text=texto))
    aux
  })
  
  fun2 <- reactive({
    switch(input$expresion2,
           "unif"= function(x) 1*(x>0 && x<1),
           "exp"= function(x) dexp(x)*(x>0),
           "norm" = function(x) dnorm(x)
    )
  })
  
  output$Grafica <- renderPlot({
    x <- seq(input$xmin, input$xmax, length.out=100)
    y1 <- sapply(x, fun1())
    y2 <- input$M*sapply(x, fun2())
    plot_limit = c(min(c(y1, y2)), max(c(y1, y2)))
    # tarea: investigar sapply, lapply, apply, tapply, mapply
    plot(x, y1, type="l", col="blue", main="GrC!fica", ylim=plot_limit)
    lines(x, y2, col="red")
    legend("topright", c("f", "M*g"), col=c("blue", "red"), lty = 1)
  })
  # 
  
  simulaciones <- reactive({
    num_aceptados <- 0
    num_intentos <- 0
    sim_Y <- switch(input$expresion2,
                    "unif"= function() runif(1),
                    "exp"= function() rexp(1),
                    "norm" = function() rnorm(1)
    )
    # print(sim_Y)
    valor_aceptados <- numeric(input$nsim)
    while(num_aceptados < input$nsim){
      Y <- sim_Y()
      U <- runif(1)
      if(Y >= input$xmin && Y<=input$xmax && U <= (fun1()(Y))/(input$M*(fun2()(Y)))){
        num_aceptados <- num_aceptados + 1
        valor_aceptados[num_aceptados] <- Y
      }
      num_intentos <- num_intentos + 1
    }
    # print(valor_aceptados)
    list(valor=valor_aceptados, tasa_exito=input$nsim/num_intentos)
  })
  
  output$tasa_exito <- renderText({
    simulaciones()$tasa_exito
  })
  
  output$hist_sim <- renderPlot({
    hist(simulaciones()$valor, main="Histograma de las simulaciones", breaks=input$nbins)
  })
  
  #------------------------------------------------------------------------
  #                  FUNCION INVERSA
  #------------------------------------------------------------------------
  

  
  rExp <- function(nsim, lambda){
    
    return((-1/lambda)*log(1-runif(nsim)))
    
  }
  
  simular <- reactive({
    seed <- set.seed(134934)
    numsim <- input$nsim
    lambd <- input$lambda
    rExp(numsim,lambd)
  })
  
  reactive_lambda <- reactive({
    input$lambda
  })
  
  
  output$simulated <- renderPlot({
  
  dat <- data.frame(Value=rExp(input$nsim, input$lambda))
  
  ggplot(dat, aes(x=Value)) +
    
    geom_histogram(aes(y=..density..), binwidth= .2, colour="black", fill="white") +
    
    stat_function(fun = function(x) input$lambda*exp(-input$lambda*x),colour = "blue")
  
})
  #------------------------------------------------------------------------
  #                  Integracion por Monte Carlo 
  #------------------------------------------------------------------------
  
  funcii <- reactive({
    texto <- paste("aux <- ", input$funci)
    eval(parse(text=texto))
    aux
  })
  
  
  mc.intervals <- function(Phi, N, X.dens=runif, alpha=al){
    
    results.list <- lapply(N, function(nsim2){
      
      X <- sapply(FUN=X.dens, nsim2) # N samples of the density of X
      PhiX <- sapply(X, Phi) # Evaluate phi at each X_i
      estim <- mean(PhiX) # Estimate of int_a^b \phi(x)f(x)df=E[phi(X_i)]
      S2 <- var(PhiX) # Estimate of the variance of phi(X_i)
      quant <- qnorm(alpha/2, lower.tail=FALSE) # Right quantile for alpha/2
      int.upper <- estim + sqrt(S2/nsim2)*quant # Upper confidence interval
      int.lower <- estim - sqrt(S2/nsim2)*quant # Lower confidence interval
      return(data.frame(N=nsim2, Estimate=estim, LI=int.lower, UI=int.upper))
      # -------
    })
    #
    results.table <- ldply(results.list) # Assembles list in data.frame
    return(results.table)
  }
  

  
  output$Grafica22<-renderPlot({
   
  nsim2<-input$nsim2
  al<-input$alpha
  set.seed(134934)
  Phi <- funcii()
  X.dens <- function(nsim2) runif(nsim2, input$a, input$b)
  N <- seq(from=input$nmin, to=input$nmax, by=100)
  data <- mc.intervals(Phi=Phi, N=N, X.dens=X.dens,al)
  data
  
  g1<-ggplot(data, aes(x=N)) +
    geom_ribbon(aes(ymin=LI, ymax=UI), fill="grey", alpha=0.4) +
    geom_line(aes(y=Estimate), colour="blue") 
  
  g1
  
  })
  
  #------------------------------------------------------------------------
  #                  Metropolis-Hastings
  #------------------------------------------------------------------------
 
  output$table <- DT::renderDataTable(DT::datatable({
    data <- p14
    data
  }))
  
output$g1<- renderPlot({
  y<- p14[,input$y]
  x<- p14[,input$x]

  plot(y~x,data=p14, 
        main="Diagrama de dispersion")
})
   







bayesfit<-function(lmfit,N){
  QR<-lmfit$qr
  df.residual<-lmfit$df.residual
  R<-qr.R(QR)
  coef<-lmfit$coef
  Vb<-chol2inv(R) 
  s2<-(t(lmfit$residuals)%*%lmfit$residuals)
  s2<-s2[1,1]/df.residual
  
  
  sigma<-df.residual*s2/rchisq(N,df.residual)
  coef.sim<-sapply(sigma,function(x) mvrnorm(1,coef,Vb*x))
  ret<-data.frame(t(coef.sim))
  names(ret)<-names(lmfit$coef)
  ret$sigma<-sqrt(sigma)
  ret
}

Bayes.sum<-function(x)
{
  c("media"=mean(x),
    "Error estandar"=sd(x),
    "t"=mean(x)/sd(x),
    "mediana"=median(x),
    "CrI"=quantile(x,prob=0.025),
    "CrI"=quantile(x,prob=0.975)
  )
}



output$bayes3<-renderPlot({
y<- p14[,input$y]
x<- p14[,input$x]
lmfit <- lm(y ~ x)

QR<-lmfit$qr
df.residual<-lmfit$df.residual
R<-qr.R(QR) ## R component
coef<-lmfit$coef
Vb<-chol2inv(R) ## variance(unscaled)
s2<-(t(lmfit$residuals)%*%lmfit$residuals)
s2<-s2[1,1]/df.residual
set.seed(134934)  ## reproducible sim

simul<-input$ns
lmfit <- lm(y ~ x)
bf<-bayesfit(lmfit,simul)
#t(apply(bf,2,Bayes.sum))
ns<-input$ns
a <- rnorm(ns,0,1)
b <- rnorm(ns,0,1)
sigma<-rnorm(ns,0.5)

gi<-density(bf$`(Intercept)`)
gx<-density(bf$x)
gsigma<-density(bf$sigma)

par(mfrow=c(3,2))
hist(a)
plot(gi)
hist(b)
plot(gx)
hist(sigma^2)
plot(gsigma)
})


output$bayes<-renderTable({
  y<- p14[,input$y]
  x<- p14[,input$x]
  lmfit <- lm(y ~ x)
  
  QR<-lmfit$qr
  df.residual<-lmfit$df.residual
  R<-qr.R(QR)
  coef<-lmfit$coef
  Vb<-chol2inv(R) ## varianza
  s2<-(t(lmfit$residuals)%*%lmfit$residuals)
  s2<-s2[1,1]/df.residual
  set.seed(134934)  # semilla
  
  simul<-input$ns
  lmfit <- lm(y ~ x)
  bf<-bayesfit(lmfit,simul)
  t(apply(bf,2,Bayes.sum))
  
  
})





output$bayes2<-renderTable({
  y<- p14[,input$y]
  x<- p14[,input$x]
  lmfit <- lm(y ~ x)
  
  QR<-lmfit$qr
  df.residual<-lmfit$df.residual
  R<-qr.R(QR)
  coef<-lmfit$coef
  Vb<-chol2inv(R) ## varianza
  s2<-(t(lmfit$residuals)%*%lmfit$residuals)
  s2<-s2[1,1]/df.residual
  set.seed(134934)  # semilla
  
  simul<-input$ns
  lmfit <- lm(y ~ x)
  bf<-bayesfit(lmfit,simul)
  bf
  
  
})
})