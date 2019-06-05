# Summary Takeaways

ending <- p("Thank you for going through the journey of American Music with us
            into unCHARTed territory! Get it, cuz music charts? :) 
            We hope you were as intrigued by what was presented and were as 
            excited as we were when we learned all of these amazing facts! As we
            end, here are some takeaways from this website:")

take_one <- p("We noticed that between the 1963 and 2019, the average album 
              length stayed pretty consistent. However, when we look at the 
              number of tracks per album, there was a slight uptick in recent
              years. Based off of these two peices of information, this means 
              that the average song length has gone down. This could be 
              contributed to many reasons, including a shorter human attention
              span, increased cost of radio airtime, cost of music production, 
              and increase in online streaming services.")

take_two <- p("While many people claim that music has lost its meaning and 
              changed what is being sung about, but based on the top 10 lyrics
              per decade, and the overall trend of lyric use from the 1960's to
              now, not a lot has changed. Based on the table below, a lot of the 
              words present are common in every decade. This includes love, 
              which was the top word of every decade, as well as girl and baby.
              Some other words have been on upward trends, such as money and 
              life. This speaks to a changing, but pretty similar, social 
              perspective displayed through music, even if some of these words,
              like baby and girl, can reinforce some of the negative 
              stereotypes in society.")

take_three <- p("")

ten_table <-tableOutput(
  outputId = "summ_top_ten"
)


summ_resp <- tabPanel(
  "Summary",
  ending,
  h3("Takeaway #1: Shorter Songs"),
  take_one,
  h3("Takeaway #2: Music Is Not Completely Different"),
  take_two,
  ten_table,
  br(),
  h3("Takeaway #3: Rock is Rockin' the Charts"),
  take_three
)