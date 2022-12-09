require(shinydashboard)
require(tidyverse)
require(shiny)
sidebar<-dashboardSidebar(sidebarMenu(
  menuItem('INTRODUCTION',tabName = 'intro'),
  menuItem('Around The World Emissions',tabName = 'graph1'),
  menuItem('Some Ideas and Observations',tabName = 'graph2'),
  menuItem('Kyoto Protocol',tabName = 'graph3'),
  menuItem('Conclusions',tabName = 'final')
)) 
ui<-dashboardPage(skin='black',
  dashboardHeader(title="CO2 Emissions"),
  sidebar,
  dashboardBody(
    tabItems(
      tabItem(tabName='intro',
              fluidRow(box(width=12,
                           title=h1(strong("CO2 Emissions: The Good, The Bad And The Ugly")),
                           img(src='factorysmoke.jpg',height=500,width=1000,alt="Smoke from a factory"))),
              fluidRow(box(width=12,
                           "One of the biggest problems plaguing the modern world is climate change. It 
                           presents many challenges, one of which is that the world's temperature has
                          increased by 1.1 degree Celsius since 1880. One of the reasons for this is levels of
                          CO2 in our atmosphere. Since 1970, CO2 emissions by humans have increased by
                          about 90%. In this project, we will look at the data to create our conclusions
                          as to why CO2 levels are rising, and what we can do about it." )),
              fluidRow(box(width=12,
                           p(strong("DATASETS:")),
                           p("-CO2: CO2 emissions of 192 United Nations Members, in 1000 Tonnes. Rows are
                             values and columns are years."),
                           p("-GDPPC: GDP per capita of various countries and others, 1960-2021."),
                           p("-renewable: Proportion of energy produced that is renewable of various
                             countries, in %.")))
              
              ),
      tabItem(tabName='graph1',
              h2(
                fluidRow(
                  box(width=12,selectInput('key',label = "Select the year for which you want the graph",
                              choices = c('Year_1994','Year_2000','Latest','Latest_Per_Capita'),
                              selected='Latest_Per_Capita')),
                  column( width=12,
                          box(width=NULL, plotOutput('googoo',height=400)),
                          box(width=NULL, "Here is a graph representing Carbon emissions
                            for multiple years in 1000T scale. 'Latest' will have values in 
                            emmisions per capita. Gray colour means there was no report
                            in the given year for the given country.")
                  )))),
      tabItem(tabName='graph2',
              h2(
                fluidRow(
                  box(title='Relation b/w GDP and Emissions',plotOutput('gdpe'),width=6),
                  box(title='Relation b/w Renewable Energy use and Emissions',plotOutput('ree'),width=6)
                ),
                fluidRow(
                  box(title='Correlation b/w GDP and Emissions',verbatimTextOutput('gdpe2'),width=6),
                  box(title='Correlation b/w use of Renewable Energy and Emissions',verbatimTextOutput('ree2'),width=6)
                ),
                fluidRow(
                  box("As you can see, Emissions are strongly correlated with GDP and weakly and inversely correlated
                      with use of renewable energy.",width=12)
                )
              )),
      tabItem(tabName='graph3',
              h2(
                fluidRow(
                  box(title = "Kyoto Protocol's impact between 1990 and 2018",
                      plotOutput('kyotop'),width=12)
                ),
                fluidRow(
                  box('In its Annex B, the Kyoto Protocol sets binding emission reduction targets
                      for 37 industrialized countries and economies in transition and the European
                      Union. Overall, these targets add up to an average 5 per cent emission
                      reduction compared to 1990 levels over the five year period 2008–2012 (the
                      first commitment period).  
                      As you can see, collaborative efforts like the kyoto protocol
                      are effective in battling climate change.',width=12)
                )
              )),
      tabItem(tabName = 'final',
                box(title = h1(strong('Conclusions')),solidHeader = TRUE,width=12,
                    h3("From the following data, we conclude that CO2 emissions vary between
                        countries, with wealthy countries emitting more CO2. This creates the problem
                        of conflicting interests, specifically that of bringing disaffected
                        populations out of poverty and of reducing carbon emissions. However, we see
                        that the use of renewable energy and pacts like the Kyoto Protocol are
                        helpful in battling the situation."))
              )
    )
  ))
server <- function(input,output){
  CO2=read.csv('CO2.csv')
  GDPPC=read.csv('GDPPC.csv')
  mapdata=read.csv('mapdata.csv')
  renew=read.csv('renew.csv')
  kyotobar=read.csv('kyotobar.csv')
  output$googoo=renderPlot({
    ggplot(mapdata,aes(x=long,y=lat,group=group))+  
      geom_polygon(aes(fill=.data[[input$key]]),color='Black')+
      scale_fill_gradientn(colors = c('green','yellow',
                                      'red','red','red','red'),name='')+
      theme(axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            aspect.ratio = 1/2,
            rect = element_blank())
  })
  output$gdpe=renderPlot({
    #Plotting
    ggplot(GDPPC,aes(x=gdp2020,y=percapita))+
      geom_point(colour='Dark Red')+
      labs(y='CO2 Emissions Per Capita, Latest',x='GDP Per Capita, 2020')+
      stat_smooth(method = "lm",
                  formula = y ~ x,
                  geom = "smooth")
  })
  output$gdpe2=renderText({cor(GDPPC$gdp2020,GDPPC$percapita)})
  output$ree=renderPlot({
    ggplot(renew,aes(renew2017,percapita))+
      geom_point(colour='blue')+
      labs(y='CO2 Emissions Per Capita, Latest',x='% Of Renewable Energy, 2017')+
      stat_smooth(method = "lm",
                  formula = y ~ x,
                  geom = "smooth")
  })
  output$ree2=renderText({cor(renew$percapita,renew$renew2017)})
  output$kyotop=renderPlot({
    ggplot(kyotobar,aes(x=years,y=sums,label=sums))+
      geom_col(fill=c('Yellow','Green'),colour='Black')+
      labs(x='Target Years',y='Pollution Levels (1000 Tonnes)')+
      theme(rect=element_blank(),axis.text.y=element_blank())+geom_label(vjust=0)
  })
}
shinyApp(ui,server)