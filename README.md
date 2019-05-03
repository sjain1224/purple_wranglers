# Project Brainstorming - Music


##Domain of Interest

We are interested in the field of __music__ because:
<ul>
  <li>
    It can signal which groups are empowered
  </li>
  <li>
    It gives insight to current social issues
  </li>
  <li>
    It is varied
  </li>
  <li>
    It is listened to by the majority of society
  </li>
  <li>
    It can demonstrate changes in society and new technology over time.
  </li>
  <li>
    It gives insight to experiences of individuals and groups of people
  </li>
</ul>


Current __examples__ of data driven projects relating to music include:

  - Jack Beckwith's "The Evolution of Muisc Genre Popularity." Which utilizes a treemap with color-coded genres demonstrating every song to reach the Hot 100 (list of best songs per given time period) as well as a line graph to trace "lifecycles" of popularity per genre before projecting, using a circular treemap, trends for 2016. To interact with these graphics, select [here](http://thedataface.com/2016/09/culture/genre-lifecycles?fbclid=IwAR2tNq0YAAklkI5TKU55t0Owx7qhoGSPUdOsp3xqkxiy1J8IWqTyELThGL4).
  - The Hip Hop Architecture camp, a nationally known project begun by Mike Ford, focuses on getting underrepresented children into the world of architecture through music and data processing. Children map out the lyrical patterns of hiphop and rap songs to different colors which can be codified against a variety of heights and shapes creating a beautiful cityscape. This is a very specific look at one song rather than a summarization of many. Check out the amazing work or Ford's, "The Hip Hop Architect," explanation of the work [here](http://hiphoparchitecture.com/hiphoparchitecture-album).
  - Kyle McDonald is also looking into music lyrics, but is analyzing the history and research on algorithm created music. It is used to "solve" writers block by utilizing all different genres' lyrics. Some turns out to be robotic (not all kinks have worked themselves out), but many are pleseant and almost difficult to distinguish from a human creation. Learn more [here](https://medium.com/artists-and-machine-intelligence/neural-nets-for-generating-music-f46dffac21c0).

The __data-driven__ questions we hope to answer about this domain are:
<ol>
  <li>
    How has Youtube influenced music?
  </li>
  <li>
    How has music changed over the decades?
  </li>
  <li>
    How has the messages music conveys changed over time
  </li>
</ol>

##Finding Data

### Data Source 1 - *Billboard 1964-2015 Songs + Lyrics*
This dataset was originally found on [this Kaggle dataset](https://www.kaggle.com/rakannimer/billboard-lyrics), which then linked to [this GitHub repository](https://github.com/walkerkq/musiclyrics). Its data was originally generated by [Billboard](https://www.billboard.com/archive/charts), who collected the data by monitoring airplay at many stations in the United States. Billboard is an American entertainment company best known for its music charts, news, videos, etc. about the most popular songs and albums in different genres; further information can be found on its [wikipedia page](https://en.wikipedia.org/wiki/Billboard_magazine).

The data is about the performance of U.S. singles every year since December 1958, using Billboard's **Year_End Hot 100** charts to create the dataset. It contains 5,100 observations (rows) and six features (columns). Each observation is information on one song. The six features are: Rank, Song, Artist, Year, Lyrics, and Source. This data can be used to answer how music has changed over the decades and how the messages music conveys have changed over time, although it's limited to singles in the U.S. and only goes back to 1958.

### Data Source 2 - *Rolling Stone's 500 Greatest Albums*
This dataset was also found on [a Kaggle dataset](https://www.kaggle.com/notgibs/500-greatest-albums-of-all-time-rolling-stone). The music data was first generated by [*Rolling Stone Magazine*](https://www.rollingstone.com/music/music-lists/500-greatest-albums-of-all-time-156826/), who conducted an extensive review process in collecting the data that include asking over 271 artists, producers, industry executives and journalists for their music opinion. As well as asking editors within their own company. Rolling Stone is an American magazine that focuses on popular culture in regards to music, film, TV, and politics. And much of their content has been influential not to its viewers but to critics as well. More information on Rolling Stone Magazine can be found on its [wikipedia page](https://en.wikipedia.org/wiki/Rolling_Stone).

Specifically, the data is about an opinionated poll, by critical music figures, of the greatest 500 albums from 1955 to 2011. It contains 500 observations (rows) and six features (columns). The observations illustrate information regarding an individual album. And the six features are denoted as: Number (album's respective rank), Year, Album, Artist, Genre, and Subgenre. Questions this data can answer vary from addressing how an album's genre fared over time and its relevance in a given a time period. It's important to note that the data is constricted to just the years of 1955 to 2011.

### Data Source 3 - *Billboard 1963-2019 Top 200 Albums Per Week*
This dataset was originally found on [this website called Components One](https://components.one/datasets/billboard-200/), which I discovered through a subReddit called R/datasets and looking up music. Their link sent me to a variety of different music datasets where I found the one I selected. Its data was originally generated by [Billboard](https://www.billboard.com/archive/charts), using their Top 200 Albums list. As said with Dataset One, Billboard is an American entertainment company best known for its music charts, news, videos, etc. about the most popular songs and albums in different genres; further information can be found on its [wikipedia page](https://en.wikipedia.org/wiki/Billboard_magazine).

The data is about the performance of U.S. albums (unlike just singles in Dataset 1) every year since January 1963, using Billboard's **Weekly Top 200 Albums** charts to create the dataset. It contains 573,948 observations (rows) and seven features (columns). Each observation is information on one album. The seven features are: Album ID, Date, Artist, Album, Rank, Number of Tracks, and Track Length (in Milliseconds). This data can be used to answer how music and artists have changed over time and even how album length has changed over time as well. It's important to note that the popularity of an album can be affected by single songs so it would also be interesting to see how the top albums match up with the top songs using dataset 1.
