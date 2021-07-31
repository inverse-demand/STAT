sig_calc <- function(sig_calc_check,
                     control_visits,
                     control_orders,
                     control_revenue,
                     challenger_visits,
                     challenger_orders,
                     challenger_revenue){
  
  if(sig_calc_check == "rpv"){
    
    test <- wilcox.test(control_revenue/control_visits, challenger_revenue/challenger_visits)
    
  } else if(sig_calc_check == "aov"){
    
    test <- wilcox.test(ifelse(is.na(control_revenue/control_orders),
                               0,
                               control_revenue/control_orders),
                        ifelse(is.na(challenger_revenue/challenger_orders),
                               0,
                               challenger_revenue/challenger_orders))
    
  } else if(sig_calc_check == "cvr"){
    
    ###
    Confidence_conversion <- function(control_orders, control_visits, challenger_orders, challenger_visits){
      
      #
      control_orders <- sum(control_orders)
      control_visits <- sum(control_visits)
      
      challenger_orders <- sum(challenger_orders)
      challenger_visits <- sum(challenger_visits)
      
      # argument values
      ctl_cvr <- control_orders/control_visits
      chl_cvr <- challenger_orders/challenger_visits
      
      # calc standard deviation
      c_std <- sqrt(ctl_cvr*(1-ctl_cvr))
      chl_std <- sqrt(chl_cvr*(1-chl_cvr))
      
      # calc standard error
      c_std_error <- c_std/sqrt(control_visits)
      chl_std_error <- chl_std/sqrt(challenger_visits)
      
      # from here is where we start to make these comparisons between the tests
      # Standard error difference calc
      st_error_diff <- sqrt((chl_std^2/challenger_visits)+(c_std^2/control_visits))
      
      # Calculate the "signal/noise"
      signal_noise <- (chl_cvr - ctl_cvr)/st_error_diff
      
      # Calculate degrees of freedom
      dof_sttest <- challenger_visits+control_visits-2
      
      # confidence <- 1 - dt(abs(signal_noise), df = dof_sttest)
      P_Value <- pt(abs(signal_noise), df = dof_sttest, lower.tail = FALSE)*2
      Confidence <- P_Value
      
      return(Confidence)
      
    }
    ###
    
    test <- Confidence_conversion(control_orders = control_orders,
                                  control_visits = control_visits,
                                  challenger_orders = challenger_orders,
                                  challenger_visits = challenger_visits)
    
  } else{
    
    print("For the sig_calc_check argument, please specify 'rpv', 'cvr', or 'aov'")
    
  }
  
  return(test)
  
}

sig_calc(sig_calc_check = "rpv",
         control_visits = control$Visits,
         control_revenue = control$Revenue,
         challenger_visits = challenger$Visits,
         challenger_revenue = challenger$Revenue)

sig_calc(sig_calc_check = "aov",
         control_orders = control$Orders,
         control_revenue = control$Revenue,
         challenger_orders = challenger$Orders,
         challenger_revenue = challenger$Revenue)

sig_calc(sig_calc_check = "cvr",
         control_orders = control$Orders,
         control_visits = control$Visits,
         challenger_orders = challenger$Orders,
         challenger_visits = challenger$Visits)