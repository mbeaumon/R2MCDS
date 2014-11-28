color.scale <-
function(col, border="black"){
             n <- length(col)
             #Create empty plot
             plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
                  axes = FALSE, xlab = "", ylab = "")
             #plot rectangles of colors
             rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
             #END of the function
             }
