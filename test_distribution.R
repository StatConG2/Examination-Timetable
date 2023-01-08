library(plotly)

trace1 <- list(
  mode = "text", 
  type = "scatter", 
  x = c(190, -30, -30, -30, -30, 570), 
  y = c(520, 472.5, 417.5, 362.5, 307.5, 520), 
  text = c("2023-01-08", "8", "9", "10", "11", "2023-01-09"), 
  textfont = list(
    size = 11, 
    family = "Arial"
  ), 
  hoverinfo = "text"
)

trace2 <- list(
  mode = "text", 
  type = "scatter", 
  x = c(95, 285, 95, 285,  95, 285, 95, 285, 475, 665, 475, 665, 475, 665, 475, 665), 
  y = c(472.5, 472.5, 417.5, 417.5, 362.5, 362.5, 307.5, 307.5, 472.5, 472.5, 417.5, 417.5, 362.5, 362.5, 307.5, 307.5), 
  text = c("STA","MAT" , "CSC", "CHE", "STA","MAT" , "CSC", "CHE", "STA","MAT" , "CSC", "CHE", "STA","MAT" , "CSC", "CHE"), 
  textfont = list(
    size = 9,
    family = "Arial"
  ), 
  hoverinfo = "text"
)

data <- list(trace1, trace2)

layout <- list(
  title = "Exam Timetable Distribution Chart",
  width = 700, 
  xaxis = list(
    showgrid = FALSE, 
    zeroline = FALSE, 
    showticklabels = FALSE
  ), 
  yaxis = list(
    showgrid = FALSE, 
    zeroline = FALSE, 
    showticklabels = FALSE
  ), 
  height = 700, 
  shapes = list(
    # box 1
    list(
      x0 = 0, 
      x1 = 190,
      y0 = 500, 
      y1 = 445, 
      line = list(width = 1), 
      type = "rect", 
      fillcolor = "rgba(250,0,0,0.5)"
    ),
    # box 2
    list(
      x0 = 190, 
      x1 = 380, 
      y0 = 500, 
      y1 = 445,
      line = list(width = 1), 
      type = "rect", 
      fillcolor = "rgba(0,250,0,0.5)"
    ),
    #box 3    
    list(
      x0 = 0, 
      x1 = 190,
      y0 = 445, 
      y1 = 390, 
      line = list(width = 1), 
      type = "rect", 
      fillcolor = "rgba(250,0,0,0.5)"
    ),
    #box 4
    list(
      x0 = 190, 
      x1 = 380, 
      y0 = 445, 
      y1 = 390, 
      line = list(width = 1), 
      type = "rect", 
      fillcolor = "rgba(0,250,0,0.5)"
    ), 
    # box 5
    list(
      x0 = 0, 
      x1 = 190,
      y0 = 390, 
      y1 = 335, 
      line = list(width = 1), 
      type = "rect", 
      fillcolor = "rgba(250,0,0,0.5)"
    ),
    # box 6
    list(
      x0 = 190, 
      x1 = 380, 
      y0 = 390, 
      y1 = 335,
      line = list(width = 1), 
      type = "rect", 
      fillcolor = "rgba(0,250,0,0.5)"
    ),
    #box 7    
    list(
      x0 = 0, 
      x1 = 190,
      y0 = 335, 
      y1 = 280, 
      line = list(width = 1), 
      type = "rect", 
      fillcolor = "rgba(250,0,0,0.5)"
    ),
    #box 8
    list(
      x0 = 190, 
      x1 = 380, 
      y0 = 335, 
      y1 = 280, 
      line = list(width = 1), 
      type = "rect", 
      fillcolor = "rgba(0,250,0,0.5)"
    ),
    # box 9
    list(
      x0 = 380, 
      x1 = 570,
      y0 = 500, 
      y1 = 445, 
      line = list(width = 1), 
      type = "rect", 
      fillcolor = "rgba(250,0,0,0.5)"
    ),
    # box 10
    list(
      x0 = 570, 
      x1 = 760, 
      y0 = 500, 
      y1 = 445,
      line = list(width = 1), 
      type = "rect", 
      fillcolor = "rgba(0,250,0,0.5)"
    ),
    #box 11    
    list(
      x0 = 380, 
      x1 = 570,
      y0 = 445, 
      y1 = 390, 
      line = list(width = 1), 
      type = "rect", 
      fillcolor = "rgba(250,0,0,0.5)"
    ),
    #box 12
    list(
      x0 = 570, 
      x1 = 760,  
      y0 = 445, 
      y1 = 390, 
      line = list(width = 1), 
      type = "rect", 
      fillcolor = "rgba(0,250,0,0.5)"
    ), 
    # box 13
    list(
      x0 = 380, 
      x1 = 570,
      y0 = 390, 
      y1 = 335, 
      line = list(width = 1), 
      type = "rect", 
      fillcolor = "rgba(250,0,0,0.5)"
    ),
    # box 14
    list(
      x0 = 570, 
      x1 = 760,  
      y0 = 390, 
      y1 = 335,
      line = list(width = 1), 
      type = "rect", 
      fillcolor = "rgba(0,250,0,0.5)"
    ),
    #box 15   
    list(
      x0 = 380, 
      x1 = 570,
      y0 = 335, 
      y1 = 280, 
      line = list(width = 1), 
      type = "rect", 
      fillcolor = "rgba(250,0,0,0.5)"
    ),
    #box 16
    list(
      x0 = 570, 
      x1 = 760,
      y0 = 335, 
      y1 = 280, 
      line = list(width = 1), 
      type = "rect", 
      fillcolor = "rgba(0,250,0,0.5)"
    )
  ), 
  hovermode = "closest", 
  showlegend = FALSE
)

p <- plot_ly(width=layout$width, height=layout$height)
p <- add_trace(p, mode=trace1$mode, type=trace1$type, x=trace1$x, y=trace1$y, text=trace1$text, textfont=trace1$textfont, hoverinfo=trace1$hoverinfo)
p <- add_trace(p, mode=trace2$mode, type=trace2$type, x=trace2$x, y=trace2$y, text=trace2$text, textfont=trace2$textfont, hoverinfo=trace2$hoverinfo)
p <- layout(p, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, shapes=layout$shapes, hovermode=layout$hovermode, showlegend=layout$showlegend)
p
