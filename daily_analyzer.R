#' BUild  analysis from a two-column dataframe
#' 
#' 

library(readr)
library(lubridate)
library(dplyr)

eom <- function(date) {
        # date character string containing POSIXct date
        date.lt <- as.POSIXlt(date) # add a month, then subtract a day:
        mon <- date.lt$mon + 2
        year <- date.lt$year
        year <- year + as.integer(mon==13) # if month was December add a year
        mon[mon==13] <- 1
        iso = ISOdate(1900+year, mon, 1, hour=0, tz="")
        result = as.POSIXct(iso) - 86400 # subtract one day
        result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst)*3600
}

holidaymerge <- function(df, holiday) {
        df <- merge(df, holiday, all.x = TRUE)
        holidayname <- names(holiday)[2]
        levels(df[[holidayname]]) <- c("N", levels(df[[holidayname]]))
        df
}

analyze <- function(daily_final, pr) {
        daily_final <- daily_final %>% 
                mutate(cdate = ymd(date)) %>% 
                mutate(datemon = substr(date, 1, 6)) %>% 
                mutate(day = day(cdate)) %>% 
                group_by(datemon) %>% 
                mutate(remainder = day(eom(cdate)) - day)
        
        pr <- pr %>%
                mutate(cdate = ymd(date)) %>%
                mutate(datemon = substr(date, 1, 6)) %>%
                mutate(day = day(cdate)) %>%
                group_by(datemon) %>%
                mutate(remainder = day(eom(cdate)) - day) 
        pr$first7 <- ifelse(pr$day < 8, pr$day, 0)
        
        daily_final <- daily_final %>% group_by(datemon) %>% mutate(subtotal = sum(su))
        daily_final <- daily_final %>% mutate(percentage = su / subtotal)
        
        daily_final <- daily_final %>% 
                mutate(first.seven = sum(percentage[day < 8])) %>% 
                mutate(last.seven = sum(percentage[remainder < 7]))
        
        daily_final$weekday <- factor(weekdays(daily_final$cdate), 
                                      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
        pr$weekday <- factor(weekdays(pr$cdate), 
                             levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
        
        daily_final$first7 <- ifelse(daily_final$day < 8, daily_final$day, 0)
        daily_final$first7 <- ifelse(daily_final$remainder < 8, daily_final$remainder, 0)
        daily_final$month <- month(daily_final$cdate)
        
        mymonths <- data.frame(month = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                               inquarter = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
                               quarter = c(3, 3, 3, 4, 4, 4, 1, 1, 1, 2, 2, 2))
        
        daily_final <- merge(daily_final, mymonths)
        
        eid_al_adha <- data.frame(cdate = c(as.Date(1:15, origin = "2012-10-18"),
                                            as.Date(1:15, origin = "2013-10-7"),
                                            as.Date(1:15, origin = "2014-9-14"),
                                            as.Date(1:15, origin = "2015-9-16"),
                                            as.Date(1:15, origin = "2016-9-4")),
                                  eid_al_adha = c(rep(paste("Eid-Al-Adha", 1:15, sep = "_"), 5)))
        deewali <- data.frame(cdate = c(as.Date(-5:3, origin = "2012-11-13"),
                                        as.Date(-5:3, origin = "2013-11-3"),
                                        as.Date(-5:3, origin = "2014-10-22"),
                                        as.Date(-5:3, origin = "2015-11-11"),
                                        as.Date(-5:3, origin = "2016-10-30"),
                                        as.Date(-5:3, origin = "2017-10-19")),
                              deewali = c(rep(paste("Deewali", -5:3, sep = "_"), 6)))
        
        
        
#        daily_final <- merge(daily_final, eid_al_adha, all.x = TRUE)
#        levels(daily_final$eid_al_adha) <- c("Not Eid", levels(daily_final$eid_al_adha))
#        daily_final[is.na(daily_final$eid_al_adha), ]$eid_al_adha <- "Not Eid"
        
        daily_final <- holidaymerge(daily_final, eid_al_adha)
        pr <- holidaymerge(pr, eid_al_adha)
        
#        daily_final <- merge(daily_final, deewali, all.x = TRUE)
#        levels(daily_final$deewali) <- c("Not Deewali", levels(daily_final$deewali))
#        daily_final[is.na(daily_final$deewali), ]$deewali <- "Not Deewali"
        
        daily_final <- holidaymerge(daily_final, deewali)
        pr <- holidaymerge(pr, deewali)
        
        daily_final[is.na(daily_final)] <- "N"
        pr[is.na(pr)] <- "N"
        
        pr$subtotal <- 25506
        
        fit <- lm(percentage ~ weekday + factor(remainder) + factor(month(cdate)) + factor(first7) + eid_al_adha + deewali + subtotal,
                   data = daily_final)
        
        
        out <- list(analyzed = daily_final, prediction = predict(fit, pr), rebuild = pr)
        
        out
        
}