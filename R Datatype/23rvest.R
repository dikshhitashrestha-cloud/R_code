library('rvest')
ls("package:rvest")
url<-"https://lbef.org/"
page<- read_html(url)
quotes<-page%>%   #chaining or pipeline operator
  html_nodes("h2,h1")%>%
  html_text()
author<-page%>%
  html_nodes("p")%>%
  html_text()

print(quotes)
print(author)