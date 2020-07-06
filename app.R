library(shiny)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(httr)
library(knitr)
library(kableExtra)
library(flextable)
library(imager)
library(tools)
library(rvest)
library(DT)
library(shinythemes)

# web scrape pokemon names to use later
link <- read_html("https://pokemondb.net/pokedex/national")
pokemon.names <- link %>%
    html_nodes("a.ent-name") %>%
    html_attr("href") %>%
    substring(., 10)
# limit to up to gen 7 since the API does; this is up to 802, and remove pokemon not in api after
pokemon.names <- pokemon.names[1:802]
# for some reason some pokemon are missing in the api; remove them
pokemon.names <- pokemon.names[-c(641, 642, 647, 650, 745, 746)]
##############################################################################
# get moves from each generation via web scrape
gen1.moves <- read_html("https://pokemondb.net/move/generation/1")
gen1.moves <- gen1.moves %>%
    html_nodes("a.ent-name") %>%
    html_attr("href") %>%
    substring(., 7) %>%
    tibble()

gen2.moves <- read_html("https://pokemondb.net/move/generation/2")
gen2.moves <- gen2.moves %>%
    html_nodes("a.ent-name") %>%
    html_attr("href") %>%
    substring(., 7) %>%
    tibble()

gen3.moves <- read_html("https://pokemondb.net/move/generation/3")
gen3.moves <- gen3.moves %>%
    html_nodes("a.ent-name") %>%
    html_attr("href") %>%
    substring(., 7) %>%
    tibble()

gen4.moves <- read_html("https://pokemondb.net/move/generation/4")
gen4.moves <- gen4.moves %>%
    html_nodes("a.ent-name") %>%
    html_attr("href") %>%
    substring(., 7) %>%
    tibble()

gen5.moves <- read_html("https://pokemondb.net/move/generation/5")
gen5.moves <- gen5.moves %>%
    html_nodes("a.ent-name") %>%
    html_attr("href") %>%
    substring(., 7) %>%
    tibble()

gen6.moves <- read_html("https://pokemondb.net/move/generation/6")
gen6.moves <- gen6.moves %>%
    html_nodes("a.ent-name") %>%
    html_attr("href") %>%
    substring(., 7) %>%
    tibble()

gen7.moves <- read_html("https://pokemondb.net/move/generation/7")
gen7.moves <- gen7.moves %>%
    html_nodes("a.ent-name") %>%
    html_attr("href") %>%
    substring(., 7) %>%
    tibble()
# combine all moves by generation into one to use later in a dropdown menu
all.moves <- list(
    gen1.moves, gen2.moves, gen3.moves, gen4.moves, gen5.moves,
    gen6.moves, gen7.moves
) %>% reduce(left_join)

all.moves <- all.moves$. %>%
  as.character(.) %>%
  gsub("-", " ", .) %>% 
  tibble()
 
################################################################################
# web scrape for items
items.link <- read_html("https://pokemondb.net/item/all")
all.items <- items.link %>%
    html_nodes("a.ent-name") %>%
    html_attr("href") %>%
    substring(., 7) %>% 
    gsub("-", " ", .)
# we need to remove the items from here that are not in gen 7 or lower; empty pics from the website indiciate what we need
# to remove as of 6/1/2020 and also remove all the item stones that end w/ x
all.items <- all.items[-c(
    6, 14, 26, 34, 53:56, 58, 59, 61, 64, 71, 84, 92, 94, 99:101, 103, 109, 111, 129, 137, 139, 141, 146,
    156:160, 163, 166, 171, 176,
    181, 185, 188:191, 194, 196, 206, 209, 210, 213, 220, 231, 238, 241:243, 248, 251, 272, 274, 275,
    277, 285, 292, 295, 300,
    301, 303, 312, 313, 317, 325, 326, 330, 338, 339, 344, 360:363, 367:369, 376, 378, 384, 394, 395,
    401, 409, 411, 412, 424, 428, 441, 444, 454, 455, 465, 470, 497, 503, 506, 510:520, 528, 530, 531, 536, 549, 558, 563,
    565, 569, 574, 581, 591, 592, 594, 598, 603, 604, 607, 610, 611, 613, 720:820, 824, 825, 826, 848
)]

#web scrape pokemon catch rates
pok.id <- 1:893

all.pok2 <- read_html("https://pokemondb.net/pokedex/national")
all.pokemon2 <- all.pok2 %>%
    html_nodes("a.ent-name") %>%
    html_attr("href") %>%
    substring(., 10) %>%
    toTitleCase(.) %>% 
    tibble() %>%
    rename("Pokemon" = ".") %>%
    cbind(pok.id) %>%
    tibble()

catch.rate.link <- read_html("https://bulbapedia.bulbagarden.net/wiki/List_of_Pok%C3%A9mon_by_catch_rate")
catch.rates <- catch.rate.link %>%
    html_nodes("td.r") %>%
    html_text("[0-9]+") %>%
    str_remove("\\n") %>%
    str_extract_all(., "[0-9]+") %>%
    as.double() %>%
    tibble() %>%
    rename("Catch Rate" = ".") %>%
    slice(1:893) %>%
    cbind(pok.id) %>%
    tibble()

#create tibble that organizes pokemon with their catch rates
catch.rates.by.pok <- left_join(catch.rates, all.pokemon2, by = "pok.id") %>%
    select(Pokemon, `Catch Rate`)
################################################################################
# Define UI for application
ui <- fluidPage(
    theme = shinytheme("sandstone"),
    # Application title formatting; make it pretty using this font from Google and use a welcome message for user
    tags$head(
        tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      h1 {
        font-family: 'Lobster', cursive;
        font-weight: 500;
        line-height: 1.1;
        color: #ff1111;
      }
      h3 {
      font-family: 'Tahoma', cursive;
            color: #191919;
      }
      h4 {
      font-family: 'Lobser', cursive;
      color: #1575ff;
      }
    ")),
        
        # Name the app
        headerPanel("PokeInfo"),
    ),
    # tab0: welcome message w/ a cute gif of pikachu
    tabsetPanel(
        tabPanel(
            "Welcome to PokeInfo!",
            # Puts a gif of Pikachu that is with given dimensions and gives a welcome message (fully static page)
            tags$img(
                height = 800,
                width = 800,
                src = "https://media.giphy.com/media/IfuEfxTfeorNS/giphy.gif"
            ),
            #give a short description of what the app does
            tags$h3("Hello and welcome to PokeInfo! You can use this app to get some basic information on Pokemon,
                         moves, and items by clicking the appropriate tabs!")
        ),
        # tab 1: get info about a pokemon
        tabPanel(
            "Pokemon Information",
            # to prepare a table showing pokemon stats
            mainPanel(
                # make user pick a pokemon name using dropdown menu; we want the table to be below the pokemon name
                selectInput(
                    "pokemon", "Pick a Pokemon to see its stats and what abilities it gets:",
                    toTitleCase(pokemon.names)
                ),
                flowLayout(
                    plotOutput("pic_front"),
                    plotOutput("shiny_front"),
                    
                    sidebarLayout(
                        tags$h4("Table of Pokemon's Stats"),
                        dataTableOutput("Pokemon")
                    )
                ),
                
                verticalLayout(
                    textOutput("type"),
                    textOutput("Abilities"),
                    
                    mainPanel(
                        plotOutput("plt")
                    )
                )
            )
        ),
        # tab 2: get info about a move
        tabPanel(
            "Move Information",
            mainPanel(
                # prepare output for information on user-chosen move using dropdown menu
                selectInput("move_name", "Pick a move to get some information about it:", toTitleCase(all.moves$.)),
                verticalLayout(
                    textOutput("Move")
                )
            )
        ),
        # tab 3: get info about an item
        tabPanel(
            "Item Information",
            mainPanel(
                selectInput("item_name", "Pick an item name for some information:", toTitleCase(all.items)),
                textOutput("Item")
            )
        ),
        #tab 4: catch rates:
        tabPanel(
            "Catch Rates",
            mainPanel(
                dataTableOutput("CatchRate")
            )
        )
    ) # ends tabset panel
) # ends UI

#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################

# Define server logic
server <- function(input, output, session) {
    #######################################################################################################
    ## tab 1
    chosen.pok <- reactive({
      # save the inputted pokemon and convert it to a string to get its stats using the link below!
      name.of.pokemon <- input$pokemon %>% str_to_lower()
      pokemon <- str_glue("https://pokeapi.co/api/v2/pokemon/{name}", name = toString(name.of.pokemon))
      pok.req <- GET(pokemon)
      # make sure it works
      stop_for_status(pok.req)
      pok.json <- content(pok.req, as = "text", encoding = "UTF-8")
      chosen.pok <- fromJSON(pok.json)
    }
    )
    # print a summary stat table of the chosen pokemon
    output$Pokemon <- DT::renderDataTable({
        # save the inputted pokemon and convert it to a string to get its stats using the link below!
        stat.name <- c("HP", "Attack", "Defense", "Special Attack", "Special Defense", "Speed")
        # give a tibble which shows each stat and what it is
        stat.table <- chosen.pok()$stats %>%
            select(base_stat) %>%
            cbind(c(
              "HP", "Attack", "Defense", "Special Attack", "Special Defense", "Speed"
            )) %>%
            tibble() %>%
            # make it easier to read by putting stat name before content
            select(base_stat) %>%
            rename("Base Stat Value" = "base_stat")
        
        stat.table <- cbind(stat.name, stat.table) %>%
            tibble() %>%
            rename("Stat Name" = "stat.name")
        stat.table
    })
    ######
    # set up images of pokemon for user to visualize which pokemon they picked
    output$pic_front <- renderPlot({
        # plot front sprite of pokemon
        title <- sprintf(
            "%s",
            input$pokemon
        )
        im <- load.image(chosen.pok()$sprites$front_default[1])
        plot(im, xaxt = "n", ann = FALSE, axes = F)
        plot(im, yaxt = "n", ann = FALSE, axes = F)
        title(main = title)
    })
    output$shiny_front <- renderPlot({
        # plot shiny front sprite of pokemon
        title <- sprintf(
            "Shiny %s",
            input$pokemon
        )
        im <- load.image(chosen.pok()$sprites$front_shiny[1])
        plot(im, xaxt = "n", ann = FALSE, axes = F)
        plot(im, yaxt = "n", ann = FALSE, axes = F)
        title(main = title)
    })
    
    # plot stat values for each stat
    output$plt <- renderPlot({
        # make a plot
      stat.name <- c("HP", "Attack", "Defense", "Special Attack", "Special Defense", "Speed")
        # give a tibble which shows each stat and what it is
        stat.table <- chosen.pok()$stats %>%
            select(base_stat) %>%
            cbind(c(
              "HP", "Attack", "Defense", "Special Attack", "Special Defense", "Speed"
            )) %>%
            tibble() %>%
            # make it easier to read by putting stat name before content
            select(base_stat) %>%
            rename("Base Stat Value" = "base_stat")
        
        stat.table <- cbind(stat.name, stat.table) %>%
            tibble() %>%
            rename("Stat Name" = "stat.name")
        bst <- sum(stat.table$`Base Stat Value`)
        title <- sprintf(
            "Stat Values for %s",
            input$pokemon
        )
        bst.label <- sprintf(
            "Base Stat Total (BST) for %s: %s",
            input$pokemon,
            bst
        )
        stat.plot <- ggplot(stat.table, aes(stat.name, `Base Stat Value`)) +
            geom_bar(
                stat = "identity", col = "springgreen4",
                fill = "thistle2"
            ) +
            xlab("Stat Name") +
            ggtitle(title) +
            labs(caption = bst.label) +
            theme(
                panel.grid.major = element_line(linetype = "blank"),
                panel.background = element_rect(fill = "paleturquoise1"),
                plot.background = element_rect(fill = "lemonchiffon2"),
                plot.caption = element_text(size = 16)
            )
        # show plot that has fixed axes to make changes more clear
        stat.plot + coord_cartesian(ylim = c(0, 275))
    })
    
    output$type <- renderPrint({
        # say which type or types depending on if it has 1 or 2 types total
        if (length(chosen.pok()$types$type$name) == 1) {
            HTML(toTitleCase(chosen.pok()$name), "is a", toTitleCase(chosen.pok()$types$type$name[1]), "type Pokemon.")
        } else {
            HTML(
                toTitleCase(chosen.pok()$name), "is a", toTitleCase(chosen.pok()$types$type$name[1]), "and",
                toTitleCase(chosen.pok()$types$type$name[2]), "type Pokemon."
            )
        }
    })
    
    output$Abilities <- renderPrint({
        abilities <- chosen.pok()$abilities$ability$name
        # check for number of abilities for grammar purposes and tell user what abilities the pokemon can get;
        # they can have two or three abilities, so we must account for those cases
        if ((length(chosen.pok()$abilities$ability$name)) == 1) {
            HTML(toTitleCase(chosen.pok()$name), "has the ability", toTitleCase(chosen.pok()$abilities$ability$name), ".")
        }
        else if (is.na(abilities[3]) == TRUE) {
            ability1 <- abilities[1]
            ability2 <- abilities[2]
            HTML(c(toTitleCase(chosen.pok()$name), paste0(
                "can have the following abilites: ", toTitleCase(ability1), " and ",
                toTitleCase(ability2), "."
            )))
        } else {
            ability1 <- abilities[1]
            ability2 <- abilities[2]
            ability3 <- abilities[3]
            HTML(c(toTitleCase(chosen.pok()$name), paste0(
                "can have the following abilites: ", toTitleCase(ability1), ", ",
                toTitleCase(ability2), ", and ", toTitleCase(ability3), "."
            )))
        }
    })
    
    #######################################################################################################
    ### tab 2
    output$Move <- renderPrint({
        # store move name to change URL after (make it reactive)
        name.of.move <- input$move_name %>% gsub(" ", "-", .) %>% str_to_lower()
        pokemon.move <- str_glue("https://pokeapi.co/api/v2/move/{name}", name = toString(name.of.move))
        move.req <- GET(pokemon.move)
        stop_for_status(move.req)
        move.json <- content(move.req, as = "text", encoding = "UTF-8")
        move.info <- fromJSON(move.json, flatten = TRUE)
        # check if the move has priority here and change 0/1 into user-friendly words
        priority.status <- move.info$priority
        pr.status <- case_when(
            priority.status == 1 ~ "a priority move",
            TRUE ~ "not a priority move"
        )
        name.of.move <- name.of.move %>% gsub("-", " ", .)
        
        # extract information about the user's chosen move; note: the \n here are to get better formatting
        # first check if it's a move with "100"% accuracy; they are guaranteed but the api doesnt treat them like this
        # set up conditionals to check for if a move does dmg and give output based on that
        if (is.null(move.info$accuracy) == TRUE & move.info$damage_class$name == "physical" | move.info$damage_class$name ==
            "special") {
            move.info$accuracy <- c(100)
        }
        
        if (move.info$damage_class$name == "status") {
            HTML(toTitleCase(name.of.move), "is a", paste0(
                toTitleCase(move.info$type$name), " type move.",
                "\nThe damage type of ", toTitleCase(name.of.move), " is ", move.info$damage_class$name,
                ".\n", toTitleCase(name.of.move), " does not do damage",
                ".\n", toTitleCase(name.of.move), " can be used ", move.info$pp, " times until there are no power points left",
                ".\n", toTitleCase(name.of.move), " is ", pr.status,
                ".\n", "Here is a description of ", toTitleCase(name.of.move), ": ", move.info$effect_entries$effect
            ))
        } else {
            HTML(
                toTitleCase(name.of.move), "is a", paste0(
                    toTitleCase(move.info$type$name), " type move.",
                    "\nThe damage type of ", toTitleCase(name.of.move), " is ", move.info$damage_class$name,
                    ".\n", toTitleCase(name.of.move), " has ", move.info$power, " base damage",
                    ".\n", toTitleCase(name.of.move), " has ", move.info$accuracy, paste0("% accuracy"),
                    ".\n", toTitleCase(name.of.move), " can be used ", move.info$pp, " times until there are no power points left",
                    ".\n", toTitleCase(name.of.move), " is ", pr.status,
                    ".\nHere is a description of ", toTitleCase(name.of.move), ": ", move.info$effect_entries$effect
                ))
        }
    })
    #######################################################################################################
    ### tab 3
    
    # get information about an item
    output$Item <- renderText({
        name.of.item <- input$item_name %>% gsub(" ", "-", .) %>% str_to_lower()
        items <- str_glue("https://pokeapi.co/api/v2/item/{name}", name = toString(name.of.item))
        items.request <- GET(items)
        stop_for_status(items.request)
        items.json <- content(items.request, as = "text", encoding = "UTF-8")
        items.content <- fromJSON(items.json) 
        # obviously, nothing costs 0 pokedollars; we will fix this by saying they are not for sale!
        if (items.content$cost == 0) {
            HTML(
                gsub("-", " ", toTitleCase(items.content$name)), "is not for sale!", paste0(
                    gsub("-", " ", toTitleCase(items.content$name)), " does the following:\n", items.content$effect_entries$short_effect
                )
            )
        } else {
            HTML(
                "A(n)", gsub("-", " ", toTitleCase(items.content$name)), "costs", prettyNum(items.content$cost,
                                                                            big.mark = ",",
                                                                            scientific = F
                ),
                paste0(
                    "pokedollars.\n",
                    gsub("-", " ", toTitleCase(items.content$name)),
                    paste0(
                        " does the following:\n", items.content$effect_entries$short_effect
                    )
                )
            )
        }
    })
    
    #tab 4: print catch rate
    output$CatchRate <- DT::renderDataTable(catch.rates.by.pok)
    
    #######################################################################################################
} # end server

# Run the application
shinyApp(ui = ui, server = server)

# references: https://rstudio.github.io/DT/shiny.html
# https://stackoverflow.com/questions/17215789/extract-a-substring-according-to-a-pattern
# Professor Chu Shing Lai's lecture notes for STA141B
