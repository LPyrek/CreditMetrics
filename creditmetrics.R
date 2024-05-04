library(readxl)
library(mvtnorm)
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

trans_prob <- data.frame(read_xlsx('projekt2_tabela.xlsx'))
return_rates <- data.frame(read_xlsx('return_rates.xlsx'))

return_rates <- lapply(return_rates, FUN = as.numeric)
return_rates <- as.data.frame(return_rates)/100


recovery_rates<- matrix(c(53.80, 51.13, 38.52, 32.74, 17.09,
                          26.86, 25.45, 23.81, 20.18, 10.90), ncol=2, nrow=5)
recovery_rates <- recovery_rates/100

corr <- c(1, 0.2, 0.15, 0.2, 1, 0.4, 0.15, 0.4, 1)
corr_matrix <- matrix(corr,3,3)

bond_rating <- c('A','B','CCC')
bond_seniority <- c('subordinated','seniorSecured','seniorUnsecured')
bond_coupon <- c(NA, 0.1, 0.2)
bond_nominal <- c(100000, 50000, 50000)
bond_maturity <- c(3,5,2)


modelCreditMetrics <- creditmetrics(0.999,trans_prob,return_rates,recovery_rates,
                       20000,corr_matrix,bond_rating,bond_seniority,
                       bond_coupon,bond_nominal,bond_maturity)

creditmetrics <- function(varLevel, trans_prob, return_rates, recovery_rates,
                          n_scenario, corr_matrix, bond_rating, bond_seniority,
                          bond_coupon, bond_nominal, bond_maturity){
  
  labels <- c('AAA','AA','A','BBB','BB','B','CCC','Def')
  
  scenarios <- rmvnorm(n_scenario, sigma = corr_matrix)
  scenarios <- as.data.frame(scenarios)
  print(scenarios)
  
  intervals <- list()
  for(i in 1:nrow(trans_prob)){
    intervals[[labels[i]]] <- trans_prob[i,]
    intervals[[i]]['upper',] <- rep(0,length(trans_prob))
    intervals[[i]]['lower',] <- rep(0,length(trans_prob))
  }
  
  # Calculate intervals
  for(j in 1:length(intervals)){
    curr <- 1
    for(i in 1:length(intervals[[1]])){
      intervals[[j]]['upper',i] <- qnorm(curr)
      curr <- curr - intervals[[j]][1,i]
      if(curr <= 0) curr <- 0
      intervals[[j]]['lower',i] <- qnorm(curr)
    }
  }
  
  scenarios_rating <- data.frame(matrix(ncol = length(bond_rating), nrow = nrow(scenarios)))
  # Rating scenarios
  for(i in 1:length(bond_rating)){
    interval <- intervals[[bond_rating[i]]]['upper',]
    interval <- rev(interval)
    interval <- append(-Inf,as.numeric(interval))
    ratings <- findInterval(scenarios[,i],interval)
    scenarios_rating[i] <- rev(labels)[ratings]
  }
  
  bond_value <- function(nominal_value, rate, coupon, maturity) {
    # Check for zero-coupon case
    if (is.na(coupon)) {
      return(nominal_value / (1 + rate[maturity-1])^(maturity-1))
    }
    
    coupon_val <- coupon*nominal_value
    coupon_pv <- coupon_val
    for(i in 1:(maturity-1)){
      coupon_pv <- coupon_pv + coupon_val/(1+rate[i])^i
    }
    face_pv <- nominal_value/(1+rate[(maturity-1)])^(maturity-1)
    return(coupon_pv + face_pv)
  }
  
  base_val <- c()
  scenarios_value <- scenarios_rating
  for(i in 1:length(bond_rating)){ #for each column
    bonds_valTable <- c() # calculate possible bonds values for each rating
    for(j in 1:nrow(return_rates)){
      bonds_valTable<-
        append(bonds_valTable,
               bond_value(bond_nominal[i],as.numeric(return_rates[j,1:(bond_maturity[i]-1)]),bond_coupon[i],bond_maturity[i]))
    }
    # base value - when ratings dont change
    base_val <- append(base_val, bonds_valTable[which(labels == bond_rating[i])])
    scenarios_value[i] <-
      apply(scenarios_rating[i], 1 , FUN = function(x){ #for each row  
        if(x == "Def"){
          m <- switch(bond_seniority[i],
                      'seniorSecured' = 1,
                      'seniorUnsecured' = 2,
                      'seniorSubordinated' = 3,
                      'subordinated' = 4,
                      'juniorSubordinated' = 5
          )
          rbeta(1,recovery_rates[m,1],recovery_rates[m,2])*bond_nominal[i]
        }else{
          bonds_valTable[which(labels == x)]
        }
      })
  }
  
  scenarios_value <- transform(scenarios_value, sumVal = rowSums(scenarios_value))
  varValue <- quantile(scenarios_value$sumVal, probs = 1 - varLevel)
  esValue <- mean(scenarios_value$sumVal[scenarios_value$sumVal < varValue])
  varLoss <- sum(base_val) - varValue
  esLoss <- sum(base_val) - esValue
  return(list(varValue = varValue,
              esValue = esValue,
              varLoss = varLoss,
              esLoss = esLoss,
              basePortfolioValue = sum(base_val),
              scenarios_value = scenarios_value,
              scenarios_rating = scenarios_rating,intervals = intervals))
}

