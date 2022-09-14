# system('make sass')
system("make style")
rstudioapi::restartSession(
  "shiny::runApp(\"main.r\")"
)
