## Main function with @ symbol.

## What it needs to do:

#### 


gild <- function(code){
 
  sliderInput("bins", "Number of bins:", 30, min = 1, max = 50)
  
  renderPlot({
    x    = faithful[, 2]  # Old Faithful Geyser data
    bins = seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  }) 
  
  
}