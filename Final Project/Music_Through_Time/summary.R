# Summary Takeaways
library(shiny)
library(knitr)

ending <- p("Thank you for going through the journey of American Music with us!
            We hope you were as intrigued by what was presented and were as 
            excited as we were when we learned all of these amazing facts! As we
            end, here are some takeaways from this website:")

take_two <- p("While many people claim that music has lost its meaning and 
              changed what is being sung about, but based on the top 10 lyrics
              per decade, and the overall trend of lyric use from the 1960's to
              now, not a lot has changed.")

ten_table <- tableOutput(
  outputId = "summ_top_ten"
)


summ_resp <- tabPanel(
  "Summary",
  ending,
  h3("Takeaway #1: "),
  h3("Takeaway #2: Music Is Not Completely Different"),
  take_two,
  kable(top_ten_words)
)