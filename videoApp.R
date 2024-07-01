#Create a dashboard to showing stats of some of the top YouTube videos over the years.
data = read_excel('Downloads/archive-4/videos-stats.xlsx')
vids = subset(data, Year == 2022)
beast = read_excel('Downloads/MrBeast_Average_Views.xlsx')
avgs = read_excel('Downloads/AvgViews and Likes.xlsx')

#Create quadrants for scatterplot
x_mid <- median(avgs$AvgViews, na.rm = TRUE)
y_mid <- median(avgs$VTL, na.rm = TRUE)

#Create navBar tabs
ui = navbarPage(
  title = 'Trending Video Dashboard',
  #Number of videos by keyword (Bar chart)
  tabPanel("Video Count Leaderboard", 
           mainPanel(plotOutput('countBar', height = '900px', width = '1800px'))
  ),
  
  #Average number of views per trending video, 2019-2022 (line graph)
  tabPanel("Average Number of Mr. Beast Views",
           mainPanel(plotOutput('beastView', height = '900px', width = '1800px'))
  ),
  #View to like ratio vs. average views by keyword (Scatterplot)
  tabPanel('Avg. Views v. VTL',
           mainPanel(plotOutput('scatter', height = '900px', width = '1800px')),
  )
  
)

#Server and plot building
server = function(input, output, session){
  #Create bar chart for all keywords
  output$countBar = renderPlot({
    ggplot(vids, aes(x = fct_rev(fct_infreq(Keyword)), 
                     fill = factor(ifelse(Keyword == 'mrbeast', 'y','n')))) + 
      geom_bar(show.legend = F) + 
      labs(title = 'Mr. Beast: Still a Leading Channel?', subtitle = 'NUMBER OF VIDEOS (2022)',
           x = 'Keyword') +
      theme(panel.background = element_blank(), 
            plot.title = element_text(face = "bold", size = 44),
            plot.subtitle = element_text(color = 'azure4', size = 24),
            axis.ticks = element_blank(), 
            axis.title = element_blank(),
            axis.text.x = element_text(
              color=c('white','white','white','white','white',
                      'white','white','white','white','white',
                      'white','white','white','white','azure4',
                      'blue','azure4','white','white','white','white',
                      'white','white','white','white','white',
                      'white','white','white','white','white',
                      'white','white','white','white','white',
                      'white','white','azure4','azure4','azure4'), 
              angle = 30, hjust = 1, vjust = 1, size = 22),
            axis.text.y = element_text(size = 15)) +
      scale_fill_manual(values = c('lightgray', 'blue')) +
      annotate('text', label = ~bold('26th'), color = 'blue', x = 16, y = 30, size = 6.5) + 
      annotate('text', label = ~bold('At first glance, Mr. Beast may'),
               color = 'black', x = 12.5, y = 38, hjust = 0, size = 10) +
      annotate('text', label = ~bold('not appear to be one of the'), color = 'black', 
               x = 12.5, y = 35.5, hjust = 0, size = 10) +
      annotate('text', label = ~bold('most popular categories.'), color = 'black', 
               x = 12.5, y = 33, hjust = 0, size = 10)
  })
  output$beastView = renderPlot({
    #Create line graph to tracks Mr. Beast's average views per video.
    ggplot(beast, aes(x = Year, y = AvgViews)) + 
      geom_point(size = 4, color = "gray") +
      geom_line(size = 1.5, color = "gray") +
      theme_classic() +
      labs(title = "Average Views per Video, 2019-2022", subtitle = "AVERAGE VIEWS/VIDEO",
           x = "",
           y = "")  +
      theme(plot.title = element_text(face = "bold", size = 44), 
            plot.subtitle = element_text(color = 'azure4', size = 24), 
            axis.line = element_line(color = 'gray'), 
            axis.title.x=element_text(colour="lightgray"),
            axis.title = element_text(size = 10),
            axis.text.x = element_text(color = 'azure4', size = 16),
            axis.text.y = element_text(color = 'azure4', size = 16),
            axis.ticks = element_blank()) +
      geom_segment(aes(x = 2021, y = 155041850, xend = 2022, yend = 28868812.61), 
                   color = "tomato1", size = 2.5)+      ####bold part of the line
      geom_point(aes(x = 2021, y = 155041850), color = "tomato1") + ###Make a point on the line
      annotate('text', x=2020.9, y=80000000,
               label = expression('The average views for Mr. Beast \n'), size = 10, color = 'gray') +
      annotate('text', x=2020.9, y=75000000,
               label = expression(bold('\n steadily climbed\n')), size = 10, color = 'azure4') +
      annotate('text', x=2020.9, y=70000000,
               label = expression('\n from 2019 to 2021, but\n'), size = 10, color = 'gray') +
      annotate('text', x=2020.9, y=65000000,
               label = expression(bold('\n fell dramatically in 2022.\n')), size = 10, color = 'tomato1') +
      scale_y_continuous(labels = scales::label_number_si())
  })
  output$scatter = renderPlot({
    #Create a scatterplot to measure VTL vs. Avg. Views
    ggplot(avgs, aes(x = AvgViews,y = VTL, color = Keyword == 'mrbeast')) + 
      geom_rect(xmin = 0, xmax = x_mid, ymin = 0, ymax = y_mid, color = '#fcfced', fill = '#fcfced') +
      geom_rect(xmin = 0, xmax = x_mid, ymin = y_mid, ymax = max(avgs$VTL), color = '#fceded', fill = '#fceded') +
      geom_rect(xmin = x_mid, xmax = max(avgs$AvgViews), ymin = 0, ymax = y_mid, color = '#f1fced', fill = '#f1fced') +
      geom_rect(xmin = x_mid, xmax = max(avgs$AvgViews), ymin = y_mid, ymax = max(avgs$VTL), color = '#fcfced', fill = '#fcfced') +
      geom_point(show.legend = F, size = 3.25) +
      geom_segment(x = x_mid, xend = x_mid, y = 0, yend = max(avgs$VTL), color = 'lightgray', linetype = 'dashed') + 
      geom_segment(x = 0, xend = max(avgs$AvgViews), y = y_mid, yend = y_mid, color = 'lightgray', linetype = 'dashed') +
      scale_x_continuous(labels = scales::label_number_si()) +
      labs(title = 'Mr. Beast in Rarified Air in Terms of Viewer Engagement',
           x = 'AVERAGE VIEWS/VIDEO (MILLIONS)', y = 'VIEW-TO-LIKE RATIO') +
      theme(panel.background = element_blank(),
            plot.title = element_text(face = "bold", size = 44),
            plot.subtitle = element_text(color = 'azure4', size = 24), 
            axis.ticks = element_blank(),
            axis.title.x = element_text(color = 'azure4', size = 18, hjust = .05),
            axis.title.y = element_text(color = 'azure4', size = 18, hjust = .95),
            axis.text = element_text(color = 'azure4', size = 15)) +
      annotate('text', label = ~bold('Mr. Beast'), x = 67200000, y = 37, color = 'blue', size = 8) +
      annotate('text', label = 'Even with the third-most Average Views,',
               x = 66590000, y = 26, color = 'blue', size = 8) +
      annotate('text', label = 'Mr. Beast had the eighth-lowest VTL ratio,',
               x = 66590000, y = 19, color = 'blue', size = 8) +
      annotate('text', label = ~bold("\ncementing his place as a premier channel."), 
               x = 66590000, y = 11.5, color = 'blue', size = 8) +
      annotate('text', label = ~bold('Google'), x = 101000000, y = 215, color = 'azure4', size = 8) +
      annotate('text', label = ~bold('Marvel'), x = 6614079, y = 28, color = 'azure4', size = 8) +
      annotate('text', label = ~bold('Animals'), x = 94723960, y = 130, color = 'azure4', size = 8) +
      annotate('text', label = "Typically, keywords with tons of views have", x = 30000000, y = 185, color = 'black', size = 8) +
      annotate('text', label = ~bold("a higher View-to-Like ratio (VTL) than smaller topics.\n"), x = 30000000, y = 167, color = 'black', size = 8) +
      annotate('text', label = "This means viewers aren't responding as positively as often.", x = 30000000, y = 171, color = 'black', size = 8) +
      scale_color_manual(values = c('azure4', 'blue'))
  })
  
}
shinyApp(ui, server)