library(shiny)
library(shinydashboard)
library(xts)
library(dplyr)
library(reshape2)
library(dygraphs)
library(bubbles)
library(mongolite)

setAWSPath<-""

setMocksKey<-c("HANLINSMMOCK","PREJUNIORMAT","PREJUNIORENG","SUMMERPREMAT","SIMULATETEST","PREEXAMTESTS","13BATTLE3800","EHANLINEXAMS")

shinyServer(function(input, output, session) {  
  
  getActivations <- reactive({
    m<-mongo(db = "ehanlin_platform_db",
             collection = "Activations",          
             url = paste0("mongodb://",setAWSPath)
    )
    
    Activations <- m$find(
      query = paste0(
        '{
          "date":{
            "$gte":{"$date":"',paste0(gsub(" ","T", paste0(as.character(input$Dates[1]-1)," 15:59:59") ),".000Z"),'"},
            "$lte":{"$date":"',paste0(gsub(" ","T", paste0(as.character(input$Dates[2])," 15:59:59") ),".000Z"),'"}
          }
        }'
      ),
      fields = '{"user" : 1 , "productSet" : 1 , "type" : 1 , "trial" : 1 , "date" : 1}'
    )
    Activations$date <- Activations$date + 8*60*60 
    return(Activations)
  })
  
  getInputUserId <- eventReactive(input$activationsRun ,{    
    m<-mongo(db = "ehanlin_platform_db",
             collection = "Users",          
             url = paste0("mongodb://",setAWSPath)
    )
    if (isolate(input$userchoice)=="email") {
      query <- paste0('{"email":"',isolate(input$inputUserMailID),'"}')
      count <- m$count(query = query)
    } else if (input$userchoice=="ID") {
      if (nchar(isolate(input$inputUserMailID))==24) {
        query <- paste0('{"_id":{"$oid":"',isolate(input$inputUserMailID),'"}}')
        count <- m$count(query = query)
      } else {
        count <- 0
      }
    }
    if (count>0) {
      Users <- m$find(
        query = query,
        fields = '{"_id":1 , "email":1 , "name":1 }'
      )
      Users$`_id` <- paste0(unlist(Users$`_id`),collapse = "")
    } else {
      Users <- NULL
      Users$`_id`<-NA
      Users$name<-NA
      Users$email<-NA
    }
    return(Users)
  })
  
  getUserActivations <- reactive({
    Users <- getInputUserId()
    
    if (!is.na(Users$"_id")) {
      withProgress(message = 'Creating Table', value = 0, {
        
        m<-mongo(db = "ehanlin_platform_db",
                 collection = "Activations",          
                 url = paste0("mongodb://",setAWSPath)
        )
        
        Activations <- m$find(
          query = paste0(
            '{
              "user":"',Users$"_id",'"
             }'
          ),
          fields = '{"user":1 , "productSet":1 , "type":1 , "trial":1 , "date":1 , "meta":1}'
        )
        
        Activations$category <- NA
        Activations$meta.payType <- NA
        Activations$meta.price <- NA
        Activations$meta.userKey <- NA
        Activations$meta.key <- NA
        if (!is.null(names(Activations$meta))) {
          if (!is.null(Activations$meta$payType)) {
            Activations$meta.payType <- Activations$meta$payType
          } 
          if (!is.null(Activations$meta$price)) {
            Activations$meta.price <- Activations$meta$price
          } 
          if (!is.null(Activations$meta$userKey)) {
            Activations$meta.userKey <- Activations$meta$userKey
          }
          if (!is.null(Activations$meta$key)) {
            Activations$meta.key <- Activations$meta$key
          }
        }
        
        incProgress(0.2, detail = "Get Activation Data")
        
        # mongo get Keys
        m<-mongo(db = "ehanlin_platform_db",
                 collection = "Keys",          
                 url = paste0("mongodb://",setAWSPath)
        )
        
        if (!is.null(names(Activations$meta))) {
          Keys <- m$find(
            query = paste0(
              '{
                "_id" : { "$in" : [',paste0('"',Activations$meta.key[!is.na(Activations$meta.key)],'"',collapse=","),'] },
                "persistent":true
             }'
            ),
            fields = '{"_id" : 1 }'
          )
        } else {
          Keys <- NULL
        }
        
        incProgress(0.2, detail = "Get Keys Data")
        
        # mongo get StatKeyTrack
        m<-mongo(db = "ehanlin_platform_db",
                 collection = "StatKeyTrack",          
                 url = paste0("mongodb://",setAWSPath)
        )
        
        StatKeyTrack <- m$find(
          query = paste0(
            '{
              "enable":true
             }'
          ),
          fields = '{"_id":1 , "salesPlan":1 , "type":1 , "category":1 }'
        )
        
        set104MockKey <- StatKeyTrack$`_id`[grep("國中模擬段考|國中模擬會考",StatKeyTrack$category)]
        
        # mongo get UserKeys
        m<-mongo(db = "ehanlin_platform_db",
                 collection = "UserKeys",          
                 url = paste0("mongodb://",setAWSPath)
        )
        
        UserKeys <- m$find(
          query = paste0(
            '{
            "user" : "',Users$"_id",'",
            "from":{"$exists":true}
      }'
          ),
          fields = '{"_id":1 , "from":1}'
          )
        if (nrow(UserKeys)==0) {
          UserKeys <- NULL
        } 
        UserKeys$from.store <- NA
        UserKeys$from.type <- NA
        if (!is.null(names(UserKeys$from))) {
          if (!is.null(UserKeys$from$store)) {
            UserKeys$from.store <- UserKeys$from$store
          }
          if (!is.null(UserKeys$from$type)) {
            UserKeys$from.type <- UserKeys$from$type
          }
        }
        
        incProgress(0.2, detail = "Get Userkey Data")
        
        setMocksKey <- unique(c(setMocksKey,StatKeyTrack$`_id`[grep("mock",StatKeyTrack$type)]))
        setKey<-unique(c(StatKeyTrack$`_id`[grep("book",StatKeyTrack$type)],setMocksKey))    
        setf3Key<-StatKeyTrack$`_id`[intersect(grep("f3",StatKeyTrack$type),grep("^y103|^y104",StatKeyTrack$salesPlan))]
        setwax3Key<-StatKeyTrack$`_id`[intersect(grep("wax3",StatKeyTrack$type),grep("^y103|^y104",StatKeyTrack$salesPlan))]
        Activations$newType<-rep(NA,nrow(Activations))    
        Activations$newType[which(Activations$type=="TRIAL")]<-"免費試用"
        Activations$newType[which(Activations$type=="KEY")]<-"實體開通"
        Activations$newType[which(Activations$type=="ONLINE")]<-"線上購物"
        Activations$newType[which(Activations$type=="ADMIN")]<-"其它"
        if (any(nchar(c(setf3Key,setwax3Key))==12)) {
          Activations$newType[which(Activations$meta.key %in% c(setf3Key,setwax3Key) )]<-"輔材開通"
        }
        Activations$newType[which(substr(Activations$meta.key,7,12) %in% c(setf3Key,setwax3Key) )]<-"輔材開通"
        Activations$newType[union(which(Activations$meta.key %in% setKey),which(substr(Activations$meta.key,7,12) %in% setKey))]<-"輔材開通"
        Activations$newType[union(which(Activations$meta.key %in% setdiff(Keys$`_id`,setKey)),which(substr(Activations$meta.key,7,12) %in% setdiff(Keys$`_id`,setKey)))]<-"其它"
        
        Activations$fromType<-UserKeys$from$type[match(Activations$meta.userKey,UserKeys$"_id")]
        Activations$fromStore<-UserKeys$from.store[match(Activations$meta.userKey,UserKeys$"_id")]
        if (length(which(Activations$newType!="實體開通"))>0) {
          Activations$fromType[which(Activations$newType!="實體開通")]<-NA
          Activations$fromStore[which(Activations$newType!="實體開通")]<-NA
        }    
        
        Activations$keyType<-rep(NA,nrow(Activations))
        if (length(which(Activations$newType=="輔材開通"))>0) {      
          Activations$keyType[which(Activations$newType=="輔材開通")]<-"獨特碼"
          Activations$keyType[which(Activations$meta.key %in% set104MockKey)] <- "公開碼"
          Activations$keyType[union(which(Activations$meta.key %in% Keys$"_id"),which(substr(Activations$meta.key,7,12) %in% Keys$"_id"))]<-"公開碼"
        }
        
        Activations$meta.payType[which(Activations$meta.payType=="chain_store_pay")]<-"便利商店繳款"
        Activations$meta.payType[which(Activations$meta.payType=="credit_card")]<-"信用卡"
        
        Activations$category[which(Activations$meta.key %in% StatKeyTrack$"_id"[nchar(StatKeyTrack$"_id")>6])] <- 
          unlist(lapply(StatKeyTrack$category[nchar(StatKeyTrack$"_id")>6],paste,collapse="|"))[
            match(
              Activations$meta.key[which(Activations$meta.key %in% StatKeyTrack$"_id"[nchar(StatKeyTrack$"_id")>6])],
              StatKeyTrack$"_id"[nchar(StatKeyTrack$"_id")>6]
            )
            ]
        
        Activations$category[is.na(Activations$category)] <-
          unlist(lapply(StatKeyTrack$category[nchar(StatKeyTrack$"_id")==6],paste,collapse="|"))[
            match(
              substr(Activations$meta.key[is.na(Activations$category)],7,12),
              StatKeyTrack$"_id"[nchar(StatKeyTrack$"_id")==6]
            )
            ]
        
        newActivationsDat<-data.frame('學年'=as.numeric(substr(Activations$productSet,2,4)),
                                      '開通課程'=Activations$productSet,
                                      '開通管道'=Activations$newType,
                                      '開通碼分類'=Activations$category,
                                      '付款方式'=Activations$meta.payType,
                                      '付款金額'=Activations$meta.price,
                                      '經銷來源'=Activations$fromType,
                                      '經銷書局'=Activations$fromStore,
                                      '輔材開通碼'=Activations$keyType,
                                      date=Activations$date+8*60*60,
                                      stringsAsFactors = F)
        newActivationsDat <- newActivationsDat[order(newActivationsDat$date,decreasing = T),]
        setProgress(1)
      })
    } else {
      newActivationsDat <- NULL
    }
    return(newActivationsDat)
  })
  
  output$trialBox <- renderValueBox({
    valueBox(
      format(length(which(getActivations()$type=="TRIAL")),format="d",big.mark=","), 
      "試用開通", icon = icon("check-square-o"), color = "blue")
  })
  
  output$onlineBox <- renderValueBox({
    valueBox(
      format(length(which(getActivations()$type=="ONLINE")),format="d",big.mark=","), 
      "線上開通", icon = icon("cloud"), color = "blue")
  })
  
  output$keyBox <- renderValueBox({
    valueBox(
      format(length(which(getActivations()$type=="KEY")),format="d",big.mark=","), 
      "實體開通", icon = icon("shopping-cart"), color = "blue")
  })
  
  output$trialProductSetBox <- renderValueBox({
    Dat <- getActivations() %>%
      subset(type=="TRIAL") %>%
      group_by(productSet) %>%
      tally() %>% 
      arrange(desc(n))
    valueBox(
      format(Dat$n[1],format="d",big.mark=","), 
      paste0("試用 ",Dat$productSet[1]), icon = icon("check-square-o"), color = "blue")
  })
  
  output$onlineProductSetBox <- renderValueBox({
    Dat <- getActivations() %>%
      subset(type=="ONLINE") %>%
      group_by(productSet) %>%
      tally() %>% 
      arrange(desc(n))
    valueBox(
      format(Dat$n[1],format="d",big.mark=","), 
      paste0("線上 ",Dat$productSet[1]), icon = icon("cloud"), color = "blue")
  })
  
  output$keyProductSetBox <- renderValueBox({
    Dat <- getActivations() %>%
      subset(type=="KEY") %>%
      group_by(productSet) %>%
      tally() %>% 
      arrange(desc(n))
    valueBox(
      format(Dat$n[1],format="d",big.mark=","), 
      paste0("實體 ",Dat$productSet[1]), icon = icon("shopping-cart"), color = "blue")
  })
  
  output$dygraph <- renderDygraph({
    Activations <-getActivations()
    Activations$date <- as.Date(Activations$date)
    ActivationsSub <- Activations %>%
      group_by(type,date) %>%
      tally() %>%
      dcast(date ~ type,value.var = "n")
    
    keySes <- zoo(ActivationsSub$KEY,ActivationsSub$date)
    onlineSes <- zoo(ActivationsSub$ONLINE,ActivationsSub$date)
    trialSes <- zoo(ActivationsSub$TRIAL,ActivationsSub$date)
    allses <- cbind(keySes,onlineSes,trialSes)
    
    dygraph(allses) %>% 
      dySeries("trialSes", label = "TRIAL",strokePattern = "dashed",fillGraph=TRUE) %>% 
      dySeries("onlineSes", label = "ONLINE",stepPlot=FALSE,fillGraph=TRUE) %>% 
      dySeries("keySes", label = "KEY",stepPlot=FALSE,fillGraph=TRUE) %>% 
      dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE) %>% 
      dyLegend(width = 400) %>% 
      dyRangeSelector()
  })
  
  output$bubblePlot <- renderBubbles({
    if (nrow(getActivations()) == 0)
      return()
    
    Dat <- getActivations() %>%
      group_by(productSet) %>%
      tally() %>% 
      arrange(desc(n)) %>%
      head(30)
    bubbles(Dat$n, Dat$productSet, key = Dat$productSet,color = "darkgray")
  })
  
  output$productSetTable <- renderTable({
    getActivations() %>%
      group_by(productSet) %>%
      tally() %>% 
      arrange(desc(n)) %>%
      select("ProductSet Name" = productSet, "開通數" = n) %>%
      as.data.frame() %>%
      head(30)
  }, digits = 1)
  
  output$userActivationsText <- renderText({
    Users <- getInputUserId()
    if (!is.na(Users$name)) {
      printText <- paste(Users$name , Users$email , sep = " | ")
    } else {
      printText <- "沒有此位使用者"
    }      
    return(printText)
  })
  
  output$userActivationsTable <- renderDataTable({
    getUserActivations()
  },options = list(lengthMenu = c(15,25,100,250)))
  
  
})
