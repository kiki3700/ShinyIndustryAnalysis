#실험할것입니다.
#패키지 설치
install.packages(c('shiny','magritt','stringr','ggplot2'))
getwd()
##패키지 실행
library(shiny)
library(magrittr)
library(stringr)
library(ggplot2)
###본코드
##데이터 불러오기
ticker<-read.csv('./data/KOR_ticker.csv',encoding = "euc-kr" ,row.names = 1)
ticker$종목코드=str_pad(ticker$종목코드,6,side=c("left"),pad='0')
ind<-ticker$fics산업군%>%unique()
#--ui
ui=fluidPage(
  titlePanel("산업별 재무재표"),
  
  #사이드바
  sidebarLayout(
    #사이드바 패널
      sidebarPanel(
       helpText("원하는 산업을 고르시요."),
         selectInput('Ind',"산업군",ind,selected='휴대폰 및 관련부품'),
       helpText('산업군내 기업을 고르시요.'),
         uiOutput("uilist")
    ),#-사이드패널
    #메인페널
  mainPanel(
    
    #탭샛패널
    tabsetPanel(
      tabPanel('IS',dataTableOutput('is')),
      tabPanel('FS', dataTableOutput('fs')),
      tabPanel('CF', dataTableOutput('cf'))
    ),#-탭샛패널
    
    #수익성지표
    div(style="background-color: coral;", h3('수익성'),
        helpText("roe"),
      fluidRow(
      column(5,
        plotOutput('roe_c', height = "200px")
      ),
      column(5,
        plotOutput('roe_i', height = "200px")
      )
    ),
    helpText("roa"),
    fluidRow(
       column(5,
             plotOutput('roa_c', height = "200px")
      ),
      column(5,
             plotOutput('roa_i', height = "200px")
      )
    ),
    helpText("opm"),
    fluidRow(
      column(5,
             plotOutput('opm_c', height = "200px")
      ),
      column(5,
             plotOutput('opm_i', height = "200px")
      )
    )
    )#-디브
    ,#수익성 지표 끝
    
    #안정성지표
    div(style="background-color: coral;", h3('안정성'),
        fluidRow(
          
          helpText("유동비율"),
          column(5,
                 plotOutput('cr_c', height = "200px")
          ),
          column(5,
                 plotOutput('cr_i', height = "200px")
          )
        ),
          fluidRow(
            
            helpText("당좌비율"),
          
          column(5,
                 plotOutput('qr_c', height = "200px")
          ),
          column(5,
                 plotOutput('qr_i', height = "200px")
          )
          ),
          fluidRow(
            
            helpText("부채비율"),
          
          
          column(5,
                 plotOutput('dr_c', height = "200px")
          ),
          column(5,
                 plotOutput('dr_i', height = "200px")
          )
        )#-플루이드 로우
        ),#-디브
    #-안정성 지표 끝
    
    #성장성지표
    div(style="background-color: coral;", h3('성장성'),
        fluidRow(
          
          helpText("매출액성장비율"),
          column(5,
                 plotOutput('si_c', height = "200px")
          ),
          column(5,
                 plotOutput('si_i', height = "200px")
          )
        ),
        fluidRow(
          
          helpText("영업이익성장비율"),
          
          column(5,
                 plotOutput('oi_c', height = "200px")
          ),
          column(5,
                 plotOutput('oi_i', height = "200px")
          )
        ),
        fluidRow(
          
          helpText("당기순이익비율"),
          
          
          column(5,
                 plotOutput('ni_c', height = "200px")
          ),
          column(5,
                 plotOutput('ni_i', height = "200px")
          )
        )#-플루이드 로우
    )#-디브
    #-성장성 지표 끝
    
    
    )#-메인패널
  )#-사이드레이아웃
)#-플루드 페이지

#--server
server = function(input,output) {

  #기업 리스트 만들기
  output$'uilist'<-renderUI({
    switch(input$'Ind',
           "휴대폰 및 관련부품"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="휴대폰 및 관련부품")],selected='삼성전자'),
           "반도체 및 관련장비"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="반도체 및 관련장비")]),
           "바이오"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="바이오")]),
           "인터넷 서비스"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="인터넷 서비스")]),
           "화학"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="화학")]),
           "자동차"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="자동차")]),
           "전자 장비 및 기기"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="전자 장비 및 기기")]),
           "개인생활용품"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="개인생활용품")]),
           "복합 산업"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="복합 산업")]),
           "자동차부품"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="자동차부품")]),
           "무선통신"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="무선통신")]),
           "게임 소프트웨어"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="게임 소프트웨어")]),
           "금속 및 광물"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="금속 및 광물")]),
           "상업은행"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="상업은행")]),
           "석유 및 가스"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="석유 및 가스")]),
           "내구소비재"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="내구소비재")]),
           "제약"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="제약")]),
           "전력"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="전력")]),
           "IT 서비스"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="IT 서비스")]),
           "보험"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="보험")]),
           "담배"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="담배")]),
           "조선"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="조선")]),
           "의료 장비 및 서비스"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="의료 장비 및 서비스")]),
           "육상운수"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="육상운수")]),
           "식료품"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="식료품")]),
           "증권"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="증권")]),
           "항공운수"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="항공운수")]),
           "호텔 및 레저"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="호텔 및 레저")]),
           "디스플레이 및 관련부품"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="디스플레이 및 관련부품")]),
           "상업서비스"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="상업서비스")]),
           "건설"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="건설")]),
           "도소매"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="도소매")]),
           "소비자 금융"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="소비자 금융")]),
           "일반 소프트웨어"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="일반 소프트웨어")]),
           "건축소재"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="건축소재")]),
           "통신장비"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="통신장비")]),
           "기계"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="기계")]),
           "미디어"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="미디어")]),
           "음료"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="음료")]),
           "가스"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="가스")]),
           "백화점"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="백화점")]),
           "해상운수"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="해상운수")]),
           "섬유 및 의복"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="섬유 및 의복")]),
           "무역"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="무역")]),
           "전기장비"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="전기장비")]),
           "운송인프라"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="운송인프라")]),
           "에너지 시설 및 서비스"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="에너지 시설 및 서비스")]),
           "건축자재"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="건축자재")]),
           "종이 및 목재"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="종이 및 목재")]),
           "온라인쇼핑"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="온라인쇼핑")]),
           "부동산"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="부동산")]),
           "용기 및 포장"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="용기 및 포장")]),
           "컴퓨터 및 주변기기"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="컴퓨터 및 주변기기")]),
           "창업투자 및 종금"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="창업투자 및 종금")]),
           "교육"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="교육")]),
           "보안장비"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="보안장비")]),
           "상호저축은행"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="상호저축은행")]),
           "가정생활용품"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="가정생활용품")]),
           "레저용품"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="레저용품")]),
           "사무기기"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="사무기기")]),
           "유선통신"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="유선통신")]),
           "셋톱 박스"=selectInput('com','기업',ticker$종목명[which(ticker$fics산업군=="셋톱 박스")])
    )
  })
  #재무제표 3총사
  output$'is'<-renderDataTable({
    a<-read.csv(paste0('./data/KOR_fs_t/',ticker[which(ticker$종목명==input$'com'),1],'_fs_t.csv'),encoding = "euc-kr")
    a[a[,1]%in%c('매출액','매출총이익','영업이익','세전계속사업이익','법인세비용','당기순이익'),]
  })  
  output$'fs'<-renderDataTable({
    a=read.csv(paste0('./data/KOR_fs_t/',ticker[which(ticker$종목명==input$'com'),1],'_fs_t.csv'),encoding = "euc-kr",stringsAsFactors=F)
    a[a[,1]%in%c('자산','유동자산','비유동자산','기타금융자산','부채','유동부채','비유동부채','기타금융업부채',
               '자본','지배기업주주지분','비지배주주지분'),]
  })  
  output$'cf'<-renderDataTable({
    a=read.csv(paste0('./data/KOR_fs_t/',ticker[which(ticker$종목명==input$'com'),1],'_fs_t.csv'),encoding = "euc-kr",stringsAsFactors=F)
    a[a[,1]%in%c('영업활동으로인한현금흐름','투자활동으로인한현금흐름','재무활동으로인한현금흐름','현금및현금성자산의증가',
               '기초현금및현금성자산','기말현금및현금성자산'),]
  })  
  #비율 구하기
  #비율.수익성
  #____roe
  output$'roe_c'<-renderPlot({
    a<-read.csv(paste0('./data/KOR_fs_t/',ticker[which(ticker$종목명==input$'com'),1],'_fs_t.csv'),encoding = "euc-kr",row.names=1,stringsAsFactors = T)
    b<-a['당기순이익',]/a['자본',]
    b<-t(b)
    b=b%>%as.data.frame()
    b$'당기순이익'<-as.numeric(b$'당기순이익')%>%round(.,4)
    b<-cbind(row.names(b),b)
    b<-setNames(b,c('date','ratio'))
    
    ggplot(b, aes(x=date,y=ratio))+
      geom_bar(position = "dodge", 
               stat = "identity")+labs(title = "기업 ROE")
    
  })
  output$'roe_i'<-renderPlot({
    a<-read.csv(paste0('./data/analysis/industry/',input$'Ind','/',input$'Ind','_fs_total.csv'),encoding = "euc-kr",row.names=1,stringsAsFactors = T)
    b<-a['당기순이익',]/a['자본',]
    b<-t(b)
    b=b%>%as.data.frame()
    b$당기순이익<-as.numeric(b$당기순이익)%>%round(.,4)
    b<-cbind(row.names(b),b)
    b<-setNames(b,c('date','ratio'))
    
    ggplot(b, aes(x=date,y=ratio))+
      geom_bar(position = "dodge", 
               stat = "identity")+labs(title = "산업 ROE ")
    
  })
  
  #___roa
  output$'roa_c'<-renderPlot({
    a<-read.csv(paste0('./data/KOR_fs_t/',ticker[which(ticker$종목명==input$'com'),1],'_fs_t.csv'),encoding = "euc-kr",row.names=1,stringsAsFactors = T)
    b<-a['당기순이익',]/a['자산',]
    b<-t(b)
    b=b%>%as.data.frame()
    b$당기순이익<-as.numeric(b$당기순이익)%>%round(.,4)
    b<-cbind(row.names(b),b)
    b<-setNames(b,c('date','ratio'))
    
    ggplot(b, aes(x=date,y=ratio))+
      geom_bar(position = "dodge", 
               stat = "identity")+labs(title = "기업 ROA")
    
  })
  output$'roa_i'<-renderPlot({
    a<-read.csv(paste0('./data/analysis/industry/',input$'Ind','/',input$'Ind','_fs_total.csv'),encoding = "euc-kr",row.names=1,stringsAsFactors = T)
    b<-a['당기순이익',]/a['자산',]
    b<-t(b)
    b=b%>%as.data.frame()
    b$당기순이익<-as.numeric(b$당기순이익)%>%round(.,4)
    b<-cbind(row.names(b),b)
    b<-setNames(b,c('date','ratio'))
    
    ggplot(b, aes(x=date,y=ratio))+
      geom_bar(position = "dodge", 
               stat = "identity")+labs(title = '산업 ROA')
    
  })
  
  #___opm
  output$'opm_c'<-renderPlot({
    a<-read.csv(paste0('./data/KOR_fs_t/',ticker[which(ticker$종목명==input$'com'),1],'_fs_t.csv'),encoding = "euc-kr",row.names=1,stringsAsFactors = T)
    b<-a['영업이익',]/a['매출액',]
    b<-t(b)
    b=b%>%as.data.frame()
    b$'영업이익'<-as.numeric(b$'영업이익')%>%round(.,4)
    b<-cbind(row.names(b),b)
    b<-setNames(b,c('date','ratio'))
    
    ggplot(b, aes(x=date,y=ratio))+
      geom_bar(position = "dodge", 
               stat = "identity")+labs(title = "기업 OPM")
    
  })
  output$'opm_i'<-renderPlot({
    a<-read.csv(paste0('./data/analysis/industry/',input$'Ind','/',input$'Ind','_fs_total.csv'),encoding = "euc-kr",row.names=1,stringsAsFactors = T)
    b<-a['영업이익',]/a['매출액',]
    b<-t(b)
    b=b%>%as.data.frame()
    b$'영업이익'<-as.numeric(b$'영업이익')%>%round(.,4)
    b<-cbind(row.names(b),b)
    b<-setNames(b,c('date','ratio'))
    
    ggplot(b, aes(x=date,y=ratio))+
      geom_bar(position = "dodge", 
               stat = "identity")+labs(title = '산업 OPM')
    
  })
  
  #비율.안정성
  #____유동성비율
  output$'cr_c'<-renderPlot({
    a<-read.csv(paste0('./data/KOR_fs_t/',ticker[which(ticker$종목명==input$'com'),1],'_fs_t.csv'),encoding = "euc-kr",row.names=1,stringsAsFactors = T)
    b<-a['유동자산',]/a['유동부채',]
    b<-t(b)
    b=b%>%as.data.frame()
    b$'유동자산'<-as.numeric(b$'유동자산')%>%round(.,4)
    b<-cbind(row.names(b),b)
    b<-setNames(b,c('date','ratio'))
    
    ggplot(b, aes(x=date,y=ratio))+
      geom_bar(position = "dodge", 
               stat = "identity")+labs(title = "기업 유동비율")
    
  })
  output$'cr_i'<-renderPlot({
    a<-read.csv(paste0('./data/analysis/industry/',input$'Ind','/',input$'Ind','_fs_total.csv'),encoding = "euc-kr",row.names=1,stringsAsFactors = T)
    b<-a['유동자산',]/a['유동부채',]
    b<-t(b)
    b=b%>%as.data.frame()
    b$'유동자산'<-as.numeric(b$'유동자산')%>%round(.,4)
    b<-cbind(row.names(b),b)
    b<-setNames(b,c('date','ratio'))
    
    ggplot(b, aes(x=date,y=ratio))+
      geom_bar(position = "dodge", 
               stat = "identity")+labs(title = "산업 유동비율")
  })
  
  #___당좌비율
  output$'qr_c'<-renderPlot({
    a<-read.csv(paste0('./data/KOR_fs_t/',ticker[which(ticker$종목명==input$'com'),1],'_fs_t.csv'),encoding = "euc-kr",row.names=1,stringsAsFactors = T)
    b<-(a['유동자산',]-a['재고자산',])/a['유동부채',]
    b<-t(b)
    b=b%>%as.data.frame()
    b$유동자산<-as.numeric(b$유동자산)%>%round(.,4)
    b<-cbind(row.names(b),b)
    b<-setNames(b,c('date','ratio'))
    
    ggplot(b, aes(x=date,y=ratio))+
      geom_bar(position = "dodge", 
               stat = "identity")+labs(title = "기업 당좌비율")
    
  })
  output$'qr_i'<-renderPlot({
    a<-read.csv(paste0('./data/analysis/industry/',input$'Ind','/',input$'Ind','_fs_total.csv'),encoding = "euc-kr",row.names=1,stringsAsFactors = T)
    b<-(a['유동자산',]-a['재고자산',])/a['유동부채',]
    b<-t(b)
    b=b%>%as.data.frame()
    b$유동자산<-as.numeric(b$유동자산)%>%round(.,4)
    b<-cbind(row.names(b),b)
    b<-setNames(b,c('date','ratio'))
    
    ggplot(b, aes(x=date,y=ratio))+
      geom_bar(position = "dodge", 
               stat = "identity")+labs(title = "산업 당좌비율")
    
  })
  
  #___부채비율
  output$'dr_c'<-renderPlot({
    a<-read.csv(paste0('./data/KOR_fs_t/',ticker[which(ticker$종목명==input$'com'),1],'_fs_t.csv'),encoding = "euc-kr",row.names=1,stringsAsFactors = T)
    b<-a['자산',]/a['부채',]
    b<-t(b)
    b=b%>%as.data.frame()
    b$'자산'<-as.numeric(b$'자산')%>%round(.,4)
    b<-cbind(row.names(b),b)
    b<-setNames(b,c('date','ratio'))
    
    ggplot(b, aes(x=date,y=ratio))+
      geom_bar(position = "dodge", 
               stat = "identity")+labs(title = "기업 부채비율")
    
  })
  output$'dr_i'<-renderPlot({
    a<-read.csv(paste0('./data/analysis/industry/',input$'Ind','/',input$'Ind','_fs_total.csv'),encoding = "euc-kr",row.names=1,stringsAsFactors = T)
    b<-a['자산',]/a['부채',]
    b<-t(b)
    b=b%>%as.data.frame()
    b$'자산'<-as.numeric(b$'자산')%>%round(.,4)
    b<-cbind(row.names(b),b)
    b<-setNames(b,c('date','ratio'))
    
    ggplot(b, aes(x=date,y=ratio))+
      geom_bar(position = "dodge", 
               stat = "identity")+labs(title = "산업 부채비율")
  })
  
  #비율.성장성
  #____매출액성장성비율
  output$'si_c'<-renderPlot({
    a<-read.csv(paste0('./data/KOR_fs_t/',ticker[which(ticker$종목명==input$'com'),1],'_fs_t.csv'),encoding = "euc-kr",row.names=1,stringsAsFactors = T)
    a<-a['매출액',1:3]
    b<-a[,2:3]-a[,1:2]
    b<-b/a[,1:2]
    b<-t(b)
    b=b%>%as.data.frame()
    b$매출액<-as.numeric(b$매출액)
    b<-cbind(row.names(b),b)
    b<-setNames(b,c('date','ratio'))
    ggplot(b, aes(x=date,y=ratio))+
      geom_bar(position = "dodge", 
               stat = "identity")+labs(title = "기업 매출성장")
    
  })
  output$'si_i'<-renderPlot({
    a<-read.csv(paste0('./data/analysis/industry/',input$'Ind','/',input$'Ind','_fs_total.csv'),encoding = "euc-kr",row.names=1,stringsAsFactors = T)
    a<-a['매출액',1:3]
    b<-a[,2:3]-a[,1:2]
    b<-b/a[,1:2]
    b<-t(b)
    b=b%>%as.data.frame()
    b$매출액<-as.numeric(b$매출액)
    b<-cbind(row.names(b),b)
    b<-setNames(b,c('date','ratio'))
    ggplot(b, aes(x=date,y=ratio))+
      geom_bar(position = "dodge", 
               stat = "identity")+labs(title = "산업 매출성장")
  })
  
  #___영업이익성장비율
  output$'oi_c'<-renderPlot({
    a<-read.csv(paste0('./data/KOR_fs_t/',ticker[which(ticker$종목명==input$'com'),1],'_fs_t.csv'),encoding = "euc-kr",row.names=1,stringsAsFactors = T)
    a<-a['영업이익',1:3]
    b<-a[,2:3]-a[,1:2]
    b<-b/a[,1:2]
    b<-t(b)
    b=b%>%as.data.frame()
    b$영업이익<-as.numeric(b$영업이익)
    b<-cbind(row.names(b),b)
    b<-setNames(b,c('date','ratio'))
    ggplot(b, aes(x=date,y=ratio))+
      geom_bar(position = "dodge", 
               stat = "identity")+labs(title = "기업 영업이익 성장")
    
  })
  output$'oi_i'<-renderPlot({
    a<-read.csv(paste0('./data/analysis/industry/',input$'Ind','/',input$'Ind','_fs_total.csv'),encoding = "euc-kr",row.names=1,stringsAsFactors = T)
    a<-a['영업이익',1:3]
    b<-a[,2:3]-a[,1:2]
    b<-b/a[,1:2]
    b<-t(b)
    b=b%>%as.data.frame()
    b$영업이익<-as.numeric(b$영업이익)
    b<-cbind(row.names(b),b)
    b<-setNames(b,c('date','ratio'))
    ggplot(b, aes(x=date,y=ratio))+
      geom_bar(position = "dodge", 
               stat = "identity")+labs(title = "산업 영업이익 성장")
    
  })
  
  #___당기순이익성장비율
  output$'ni_c'<-renderPlot({
    a<-read.csv(paste0('./data/KOR_fs_t/',ticker[which(ticker$종목명==input$'com'),1],'_fs_t.csv'),encoding = "euc-kr",row.names=1,stringsAsFactors = T)
    a<-a['당기순이익',1:3]
    b<-a[,2:3]-a[,1:2]
    b<-b/a[,1:2]
    b<-t(b)
    b=b%>%as.data.frame()
    b$당기순이익<-as.numeric(b$당기순이익)
    b<-cbind(row.names(b),b)
    b<-setNames(b,c('date','ratio'))
    ggplot(b, aes(x=date,y=ratio))+
      geom_bar(position = "dodge", 
               stat = "identity")+labs(title = "기업 당기순이익 성장")
    
  })
  output$'ni_i'<-renderPlot({
    a<-read.csv(paste0('./data/analysis/industry/',input$'Ind','/',input$'Ind','_fs_total.csv'),encoding = "euc-kr",row.names=1,stringsAsFactors = T)
    a<-a['당기순이익',1:3]
    b<-a[,2:3]-a[,1:2]
    b<-b/a[,1:2]
    b<-t(b)
    b=b%>%as.data.frame()
    b$당기순이익<-as.numeric(b$당기순이익)
    b<-cbind(row.names(b),b)
    b<-setNames(b,c('date','ratio'))
    ggplot(b, aes(x=date,y=ratio))+
      geom_bar(position = "dodge", 
               stat = "identity")+labs(title = "산업 당기순이익 성장")
  })
  
}

shinyApp(ui, server)
  
