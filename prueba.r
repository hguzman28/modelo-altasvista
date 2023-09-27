library(rvest)
library(data.table)
# Parameters
email<-"reservasgrupoaltasvistas3@gmail.com"
pass<-"123456"
initial_date <- "2023-06-01"
final_date <- "2023-06-30"

# Create a session and simulate a login
session <-session("https://www.covermanager.com/login?lang=es")
login_form<-html_form(session)[[1]]
filled_form<-html_form_set(login_form, email=email, password=pass)
session_open<-session_submit(session,filled_form) # Submit the filled form to login

# Navigate to a specified url 
# url<-paste0("https://www.covermanager.com/Stats/reserv_stats_basic/all/",initial_date,"/",final_date,"/table/this/no/date")
url<-paste0("https://www.covermanager.com/Stats/reserv_stats/all/",initial_date,"/",final_date,"/table/all/yes/no/null/no/date") #consulta con mas campos
session_open<-session_jump_to(session_open,url)

# Scrape html_table from the logged in session
Tracking_reservas<- as.data.table(html_table(session_open)) 
Tracking_reservas