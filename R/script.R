library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

pop <- 100
id <- 1:pop

ptSalary <- 5 + 6
ftSalary <- 10
tpSalary <- 30
diffMenWomen <- 1

load("dat.Rdata")
load("oddsList.Rdata")

calculateLabor <- function(ptBenefit
                           , tpSalary
                           , diffMenWomen)
{
    if(diffMenWomen == 0) 
    {
        sex <- rep("either", length(id))
    } else {
        sex <- c(rep("man", diffMenWomen)
                 , rep(c("woman", "man"), length(id)/2-diffMenWomen)
                 , rep("woman", diffMenWomen))
    }
    
    cutoff <- ptBenefit / tpSalary
    
    marginalPerson <- last(filter(dat, oddsMargin > cutoff)$id)
    
    labor <- cbind(dat, sex) %>%
        select(-oddsMargin) %>%
        full_join(oddsList[[marginalPerson]]) %>% 
        mutate(fulltime = ifelse(odds > 0, TRUE, FALSE),
               parttime = !fulltime)
    
    return(labor)
}

plotLabor <- function(labor)
{
    sums <- labor %>%
        group_by(sex) %>%
        summarise(topPositions = sum(odds),
                  fulltime = sum(fulltime),
                  parttime = sum(parttime)) %>%
        ungroup()
    
    number <- sums %>%
        mutate(type = "number") %>%
        gather(variable, value
               , topPositions:parttime
               , factor_key = TRUE)
    
    share <- sums %>%
        mutate(type = "share") %>%
        mutate(topPositions = topPositions / sum(topPositions) * 100,
               fulltime = fulltime / sum(fulltime) * 100,
               parttime = parttime / sum(parttime) * 100) %>%
        gather(variable, value
               , topPositions:parttime
               , factor_key = TRUE)
    
    plotData <- bind_rows(number, share) %>%
        mutate(value = round(value))
    
    ggplot(plotData, aes(sex, value, fill = sex)) +
        facet_grid(type ~ variable, scales = "free_y") +
        geom_col() + 
        geom_text(aes(label = value, vjust = 1))
}

testLabor <- function()
{
    labor <- calculateLabor(ptSalary
                            , ftSalary
                            , tpSalary
                            , diffMenWomen)
    
    plotLabor(labor)
}

# Update probabilities?
if(FALSE)
{
    set.seed(1)
    
    talent <- id[pop:1]
    tpN <- 10
    
    oddsMargin <- rep(0, pop)
    
    loops <- 100000
    
    dat <- data.frame(id, talent, oddsMargin)
    oddsList <- vector("list", pop)
    
    for(p in 1:(tpN-1))
    {
        dat[id == p,] <- filter(dat, id == p) %>% mutate(oddsMargin = 1)
        oddsList[[p]] <- mutate(dat, odds = ifelse(p <= id, 0, 1))
    }
    
    for(p in tpN:pop) 
    {
        person <- dat[id == p,]
        
        lottery <- dat %>% filter(talent >= person$talent)
        
        winners <- rep(NA, length = tpN * loops)
        
        for(q in 1:loops-1) 
        {
            winners[(q*tpN+1):((q*tpN)+tpN)] <- 
                sample(lottery$id
                       , size = tpN
                       , prob = lottery$talent)
        }
        
        odds <- c(as.data.frame(table(winners))$Freq / loops
                  , rep(0, pop-p))
        
        oddsList[[p]] <- cbind(select(dat, -oddsMargin), odds)
        
        person$oddsMargin = sum(winners == person$id) / loops
        
        message(paste0("Person "
                       , person$id
                       , " with talent "
                       , person$talent
                       , " has a probability of "
                       , format(person$oddsMargin, nsmall = 3)
                       , " at the margin."))
        
        dat[id == p,] <- person
    }
    
    # Save to disk
    save(dat,      file = "dat.Rdata")
    save(oddsList, file = "oddsList.Rdata")
} 
