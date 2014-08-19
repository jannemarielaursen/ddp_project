# developing data products shiny server.R

# Loading libraries
library(shiny)
library(ggplot2)
library(RColorBrewer)

# defining functions to define group vectors
compute.age.group <- function(age){
    age <- as.numeric(age)
    if (age < 35){
        age.vec <- c(1, 0, 0, 0, 0)
    }
    else if (age < 45){
        age.vec <- c(0, 1, 0, 0, 0)
    }
    else if (age < 55){
        age.vec <- c(0, 0, 1, 0, 0)
    }
    else if (age < 65){
        age.vec <- c(0, 0, 0, 1, 0)
    }
    else {
        age.vec <- c(0, 0, 0, 0, 1)
    }
    return(age.vec)
}
compute.gender.group <- function(gen){
    if (gen == 'M'){
        gen.vec <- c(0, 1)
    }
    else {
        gen.vec <- c(1, 0)
    }
    return(gen.vec)
}
compute.descent.group <- function(des){
    if (des == 'Other'){
        des.vec <- c(1, 0, 0, 0)
        dec <- 'A'
    }
    else if (des == 'Asian'){
        des.vec <- c(0, 1, 0, 0)
        dec <- 'B'
    }
    else if (des %in% c("Aboriginal", "Torres Islander")){
        des.vec <- c(0, 0, 0, 1)
        dec <- 'D'
    }
    else {
        des.vec <- c(0, 0, 1, 0)
        dec <- 'C'
    }
    return(c(des.vec, dec))
}
compute.born.group <- function(bor){
    hi.risk.places <- c("Asia", "Southern Europe", "Middle East", "Africa - north of Sahara")
    if (bor %in% hi.risk.places){
        bor.vec <- c(0, 1)
    }
    else{
        bor.vec <- c(1, 0)
    }
    return(bor.vec)
}
compute.yes.group <- function(fam){
    if (fam =='Yes'){
        fam.v <- c(0, 1)
    }
    else{
        fam.v <- c(1, 0)
    }
    return(fam.v)
}
compute.no.group <- function(fam){
    if (fam =='No'){
        fam.v <- c(0, 1)
    }
    else{
        fam.v <- c(1, 0)
    }
    return(fam.v)
}
compute.waist.group <- function(gender, descent, waist, waistunit){
    if (gender == 'M'){
        if (descent %in% c("B", "D")) {
            lower <- 90
            upper <- 100
        }
        else{
            lower <- 102
            upper <- 110
        }
    }
    else{
        if (descent %in% c("B", "D")) {
            lower <- 80
            upper <- 90
        }
        else{
            lower <- 88
            upper <- 100
        }
    }
    
    waist <- as.numeric(waist)
    if (waistunit == 'in'){
        waist <- waist *2.54
    }
    
    if (waist < lower){
        waist.risk <- c(1, 0, 0)
    }
    else if (waist < upper){
        waist.risk <- c(0, 1, 0)
    }
    else{
        waist.risk <- c(0, 0, 1)
    }
    return(waist.risk)
}
# compute vector with loadings for the risk factors
compute.data.vector <- function(age, gen, des, bor, fam, his, bpm, smo, fru, exe, wai, wau){
    # age
    age.v <- compute.age.group(age)
    # gender
    gen.v <- compute.gender.group(gen)    
    # descent
    temp.des <- compute.descent.group(des)
    des.v <- temp.des[1:4]
    dec <- temp.des[5]
    # born
    bor.v <- compute.born.group(bor)
    # Yes/No answers
    fam.v <- compute.yes.group(fam)
    his.v <- compute.yes.group(his)
    bpm.v <- compute.yes.group(bpm)
    smo.v <- compute.yes.group(smo)
    fru.v <- compute.no.group(fru)
    exe.v <- compute.no.group(exe)
    # waist
    wai.v <- compute.waist.group(gen, dec, wai, wau)
    return(as.numeric(c(age.v, gen.v, des.v, bor.v, fam.v, his.v, bpm.v, smo.v, fru.v, exe.v, wai.v)))
}

# Define scoring vector
age.points <- c(0, 2, 4, 6, 8) # below 35, 35-44, 45-54, 55-64, 65 and above
gender.points <- c(0, 3) # F, M
descent.points <- c(0, 0, 2, 2) # A, B[Asian], C, D[Torres Strait, Aboriginal]
born.points <- c(0, 2) # low risk, high risk
family.points <- c(0, 3) # no, yes
history.points <- c(0, 6) # no, yes
bpmed.points <- c(0, 2) # no, yes
smoke.points <- c(0, 2) # no, yes
fruit.points <- c(0, 1) # yes, no
exercise.points <- c(0, 2) # yes, no
waist.points <- c(0, 4, 7) # lo, mid, hi risk

# Making a combined scoring vector
all.points <- c(age.points, gender.points, descent.points, born.points, 
                family.points, history.points, bpmed.points, smoke.points, 
                fruit.points, exercise.points, waist.points)

# Define reference risk categories
baseline <- data.frame(data.source=rep('Population risk category', 3), trait=c('Low risk', 'Medium risk', 'High risk'), 
                       trait.score=c(5.5, 6.0, 26.5), trait.label=c(2.5,8.5, 25.0))

# set the plotting color palette
more.palette1 <- c('#ccebc5', '#fed9a6', '#fbb4ae', '#b3cde3', '#decbe4')


shinyServer(function(input, output) {
    
    output$score <- renderPlot({
        if(input$goButton) { 
            # get data from input if updated (after inital submit)
            input.vector <- c(input$age, input$gender, input$descent, input$born, 
                              input$family, input$history, input$bpmed, input$smoke, 
                              input$fruit, input$exercise, input$waist, input$waistunit)
            # calculate type 2 diabetes risk scores
            score.vec <- all.points*do.call(compute.data.vector, as.list(input.vector))
            diabetes.scores <- sum(score.vec)
            invariable.trait <- sum(score.vec[1:17])
            score.variable.trait <- sum(score.vec[18:28])
            # create dataframe
            t2d.data <- rbind(data.frame(data.source=rep('Your score', 2), trait=c('Fixed trait', 'Variable trait'), 
                                         trait.score=c(invariable.trait, score.variable.trait), 
                                         trait.label=c(
                                             ifelse(invariable.trait==0, 0, 0.5*invariable.trait), 
                                             ifelse(score.variable.trait==0, 0, invariable.trait+0.5*score.variable.trait))),
                              baseline)
            t2d.data$trait <- factor(t2d.data$trait, levels=c('Low risk', 'Medium risk', 'High risk', 'Fixed trait', 'Variable trait'))
            # Make plot
            p <- ggplot(data = t2d.data, aes(x = data.source, y = trait.score, fill=trait))
            p <- p + geom_bar(stat='identity', position='stack', color='black', width=0.5) 
            p <- p + scale_fill_manual(values = more.palette1, name='Risk category', guide=FALSE) 
            p <- p + geom_text(aes(label = ifelse(trait.label==0, '', levels(trait)[trait]), y = trait.label), size = 4)
            p <- p + geom_hline(yintercept=c(invariable.trait, diabetes.scores)) 
            p <- p + geom_text(data = NULL, x = 1.5, y=diabetes.scores, label='Your risk score', vjust=-0.5) 
            p <- p + geom_text(data = NULL, x = 1.5, y=invariable.trait, 
                          label=ifelse(diabetes.scores == invariable.trait, 'Your risk score', 'Your baseline risk'), vjust=-0.5)
            p <- p + theme_bw()
            p <- p + xlab(NULL) 
            p <- p + ylab("AUSDRISK diabetes score") 
            p <- p + scale_y_continuous(breaks=seq(0, 38, by=1))
            p
        }
    })
    
#     output$values <- renderTable({
#         # Take a dependency on input$goButton
#         if(input$goButton) {        
#         sliderValues()
#         }
#     })

})