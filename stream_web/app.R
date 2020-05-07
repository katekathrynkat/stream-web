# Load packages
library(shiny)
library(tidyverse)

# Load data
dat <- read_csv('data/Cooper2015_fig8_data.csv') %>% 
    mutate(treatment = factor(treatment, levels = c('UNT', 'UNTL', 'BRI', 'BRB')))

# Plot elements
cols <- c('green', 'blue', 'orange', 'red')
names(cols) <- levels(dat$treatment)

# UI
ui <- fluidPage(
    
    # Title
    titlePanel('Stream Web'),
    
    # Header box
    fluidRow(
        column(12,
               wellPanel(
                   p('Data from figure 8 in \"Physicochemical and biological responses of streams to wildfire severity in riparian zones\" (Cooper et al. 2015). Bars represent the average values for resources and their consumers in the riffles of sites in different trout and basin burn categories, June 2010.'),
                   radioButtons('treat',
                                'Which treatment would you like to compare to the \"Unburned - trout\" treatment?',
                                c('Burned - riparian intact'='BRI',
                                  'Burned - riparian burned'='BRB',
                                  'Unburned - no trout'='UNTL'))
               ))
    ),
    
    # Predators
    p('PREDATORS'),
    fluidRow(
        column(3),
        column(6,
               splitLayout(plotOutput('predator_plot', height = 120, width = 140),
                           plotOutput('micropredator_plot', height = 120, width = 140))
              )
    ),
    hr(),
    
    # Primary consumers
    p('PRIMARY CONSUMERS'),
    fluidRow(
        column(12,
               splitLayout(plotOutput('algivore_big_plot', height = 120, width = 140),
                           plotOutput('algivore_plot', height = 120, width = 140),
                           plotOutput('filter_feeder_plot', height = 120, width = 140),
                           plotOutput('shredder_plot', height = 120, width = 140),
                           plotOutput('deposit_feeder_plot', height = 120, width = 140))
        )
    ),
    hr(),
    
    # Algae and detritus
    p('ALGAE & DETRITUS'),
    fluidRow(
        column(12,
               splitLayout(plotOutput('chla_hard_plot', height = 120, width = 140),
                           plotOutput('chla_soft_plot', height = 120, width = 140),
                           plotOutput('macroalgae_plot', height = 120, width = 140),
                           plotOutput('cpom_plot', height = 120, width = 140),
                           plotOutput('fpom_plot', height = 120, width = 140))
        )
    ),
    hr(),
    
    # Abiotic factors
    p('ABIOTIC FACTORS'),
    fluidRow(
        column(3),
        column(6,
               splitLayout(plotOutput('canopy_plot', height = 120, width = 140),
                           plotOutput('temp_plot', height = 120, width = 140))
        )
    )
    
)

# Server
server <- function(input, output) {

    # Algivores
    output$algivore_plot <- renderPlot({
        x <- filter(dat, level=='algivore', treatment=='UNT'|treatment==input$treat)
        ggplot(x, aes(x = treatment, y = mean, fill = treatment)) +
            geom_col() +
            scale_fill_manual(values = cols) +
            scale_y_continuous(expand = c(0,0), limits = c(0,7)) + 
            theme_classic() + theme(legend.position = 'none', axis.title.x = element_blank()) + 
            labs(title = 'Algivores', y = 'thousands/m^2')
    })
    
    # Algivores >1mm
    output$algivore_big_plot <- renderPlot({
        x <- filter(dat, level=='algivore_big', treatment=='UNT'|treatment==input$treat)
        ggplot(x, aes(x = treatment, y = mean, fill = treatment)) +
            geom_col() +
            scale_fill_manual(values = cols) +
            scale_y_continuous(expand = c(0,0), limits = c(0,1500)) + 
            theme_classic() + theme(legend.position = 'none', axis.title.x = element_blank()) + 
            labs(title = 'Algivores >1mm', y = 'indv/m^2')
    })
    
    # Canopy cover
    output$canopy_plot <- renderPlot({
        x <- filter(dat, level=='canopy', treatment=='UNT'|treatment==input$treat)
        ggplot(x, aes(x = treatment, y = mean, fill = treatment)) +
            geom_col() +
            scale_fill_manual(values = cols) +
            scale_y_continuous(expand = c(0,0), limits = c(0,100)) + 
            theme_classic() + theme(legend.position = 'none', axis.title.x = element_blank()) + 
            labs(title = 'Canopy Cover', y = '%')
    })
    
    # Chl-a (hard)
    output$chla_hard_plot <- renderPlot({
        x <- filter(dat, level=='chla_hard', treatment=='UNT'|treatment==input$treat)
        ggplot(x, aes(x = treatment, y = mean, fill = treatment)) +
            geom_col() +
            scale_fill_manual(values = cols) +
            scale_y_continuous(expand = c(0,0), limits = c(0,50)) + 
            theme_classic() + theme(legend.position = 'none', axis.title.x = element_blank()) + 
            labs(title = 'Chl-a (hard)', y = 'mg/m^2')
    })
    
    # Chl-a (soft)
    output$chla_soft_plot <- renderPlot({
        x <- filter(dat, level=='chla_soft', treatment=='UNT'|treatment==input$treat)
        ggplot(x, aes(x = treatment, y = mean, fill = treatment)) +
            geom_col() +
            scale_fill_manual(values = cols) +
            scale_y_continuous(expand = c(0,0), limits = c(0,110)) + 
            theme_classic() + theme(legend.position = 'none', axis.title.x = element_blank()) + 
            labs(title = 'Chl-a (soft)', y = 'mg/m^2')
    })
    
    # CPOM
    output$cpom_plot <- renderPlot({
        x <- filter(dat, level=='cpom', treatment=='UNT'|treatment==input$treat)
        ggplot(x, aes(x = treatment, y = mean, fill = treatment)) +
            geom_col() +
            scale_fill_manual(values = cols) +
            scale_y_continuous(expand = c(0,0), limits = c(0,8)) + 
            theme_classic() + theme(legend.position = 'none', axis.title.x = element_blank()) + 
            labs(title = 'CPOM', y = 'AFDM (g/m^2)')
    })
    
    # Deposit-feeders
    output$deposit_feeder_plot <- renderPlot({
        x <- filter(dat, level=='deposit_feeder', treatment=='UNT'|treatment==input$treat)
        ggplot(x, aes(x = treatment, y = mean, fill = treatment)) +
            geom_col() +
            scale_fill_manual(values = cols) +
            scale_y_continuous(expand = c(0,0), limits = c(0,4)) + 
            theme_classic() + theme(legend.position = 'none', axis.title.x = element_blank()) + 
            labs(title = 'Deposit-feeders', y = 'thousands/m^2')
    })
    
    # Filter-feeders
    output$filter_feeder_plot <- renderPlot({
        x <- filter(dat, level=='filter_feeder', treatment=='UNT'|treatment==input$treat)
        ggplot(x, aes(x = treatment, y = mean, fill = treatment)) +
            geom_col() +
            scale_fill_manual(values = cols) +
            scale_y_continuous(expand = c(0,0), limits = c(0,1500)) + 
            theme_classic() + theme(legend.position = 'none', axis.title.x = element_blank()) + 
            labs(title = 'Filter-feeders', y = 'indv/m^2')
    })
    
    # FPOM
    output$fpom_plot <- renderPlot({
        x <- filter(dat, level=='fpom', treatment=='UNT'|treatment==input$treat)
        ggplot(x, aes(x = treatment, y = mean, fill = treatment)) +
            geom_col() +
            scale_fill_manual(values = cols) +
            scale_y_continuous(expand = c(0,0), limits = c(0,1.5)) + 
            theme_classic() + theme(legend.position = 'none', axis.title.x = element_blank()) + 
            labs(title = 'FPOM', y = 'AFDM (g/m^2)')
    })
    
    # Macroalgae
    output$macroalgae_plot <- renderPlot({
        x <- filter(dat, level=='macroalgae', treatment=='UNT'|treatment==input$treat)
        ggplot(x, aes(x = treatment, y = mean, fill = treatment)) +
            geom_col() +
            scale_fill_manual(values = cols) +
            scale_y_continuous(expand = c(0,0), limits = c(0,60)) + 
            theme_classic() + theme(legend.position = 'none', axis.title.x = element_blank()) + 
            labs(title = 'Macroalgae', y = '% Cladophora Cover')
    })
    
    # Micropredators
    output$micropredator_plot <- renderPlot({
        x <- filter(dat, level=='micropredator', treatment=='UNT'|treatment==input$treat)
        ggplot(x, aes(x = treatment, y = mean, fill = treatment)) +
            geom_col() +
            scale_fill_manual(values = cols) +
            scale_y_continuous(expand = c(0,0), limits = c(0,1.5)) + 
            theme_classic() + theme(legend.position = 'none', axis.title.x = element_blank()) + 
            labs(title = 'Micropredators', y = 'thousands/m^2')
    })
    
    # Predators
    output$predator_plot <- renderPlot({
        x <- filter(dat, level=='predator', treatment=='UNT'|treatment==input$treat)
        ggplot(x, aes(x = treatment, y = mean, fill = treatment)) +
            geom_col() +
            scale_fill_manual(values = cols) +
            scale_y_continuous(expand = c(0,0), limits = c(0,500)) + 
            theme_classic() + theme(legend.position = 'none', axis.title.x = element_blank()) + 
            labs(title = 'Predators', y = 'indv/m^2')
    })
    
    # Shredders
    output$shredder_plot <- renderPlot({
        x <- filter(dat, level=='shredder', treatment=='UNT'|treatment==input$treat)
        ggplot(x, aes(x = treatment, y = mean, fill = treatment)) +
            geom_col() +
            scale_fill_manual(values = cols) +
            scale_y_continuous(expand = c(0,0), limits = c(0,900)) + 
            theme_classic() + theme(legend.position = 'none', axis.title.x = element_blank()) + 
            labs(title = 'Shredders', y = 'indv/m^2')
    })

    # Water Temperature
    output$temp_plot <- renderPlot({
        x <- filter(dat, level=='temp', treatment=='UNT'|treatment==input$treat)
        ggplot(x, aes(x = treatment, y = mean, fill = treatment)) +
            geom_col() +
            scale_fill_manual(values = cols) +
            scale_y_continuous(expand = c(0,0), limits = c(0,20)) + 
            theme_classic() + theme(legend.position = 'none', axis.title.x = element_blank()) + 
            labs(title = 'Water Temperature', y = 'deg C')
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
