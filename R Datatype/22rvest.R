library('rvest')
ls("package:rvest")
url<-"https://quotes.toscrape.com/"
page<- read_html(url)
quotes<-page%>%   #chaining or pipeline operator
  html_nodes(".text")%>%
  html_text()
author<-page%>%
  html_nodes(".author")%>%
  html_text()
data<-data.frame(Quotes=quotes, Author=author)
print(data)