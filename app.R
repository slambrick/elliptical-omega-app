#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(pracma)
library(markdown)

#================ Cosine modified functions ===================================#

#================ Solid angle functions =======================================#

# Integrand for the general case from 'John T. Conway' 'Analytic solution
# for the solid angle subtended at a point source radiation vector potential'
# which claims to be general, unlike Abbas et.al. who do not consider the
# case inside the bounding box but outisde the ellipse."""
integrand_general <- function(phi, p, q, h, a, b) {
    tmp = p*p + q*q + h*h + 2*a*p*cos(phi) + 2*b*q*sin(phi) + 
        (a*cos(phi))^2 + (b*sin(phi))^2
    term1 = 1 - h/(sqrt(tmp))
    tmp = tmp - h*h
    term2 = (a*b + p*b*cos(phi) + q*a*sin(phi))/(tmp)
    result = term1*term2
    return(result)
}

# Calculates the solid angle using the formula derived by John T. Conway
# 2010. Is faster than the alternative integral in 'solid_angle_calc'.
#
# Inputs:
#   p - The distance between the point of interest and the centre of the
#       ellipse, projected into the plane of the ellipse along the major axis.
#       Should not be negative.
#   q - The distance between the point of interest and the centre of the
#       ellipse, projected into the plane of the elllipse along the minor
#       axis. Should not be negative
#   h - The perpendicular distance between the point of interest and the plane
#       of the ellipse. Should not be negative.
#   a - The semi-axis of the ellipse along which the point of interest lies
#       when it is projected into the plane of the ellipse.
#   b - The other semi-axis of the ellipse.
#
# Output:
#   omega - The solid angle subtended by the ellipse from the point of
#           interest
calc_omega <- function(p, q, h, a, b) {
    f <- function(th) integrand_general(th, p, q, h, a, b)
    omega = integral(f, 0, 2*pi)
    return(omega)
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Elliptical solid angle calculator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("p",
                        "p = ",
                        min = 0,
                        value = 0),
            numericInput("q",
                         "q = ",
                         min = 0,
                         value = 0),
            numericInput("h",
                         "h = ",
                         min = 0,
                         value = 0),
            numericInput("a",
                         "a = ",
                         min = 0,
                         value = 0),
            numericInput("b",
                         "b = ",
                         min = 0,
                         value = 0),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            includeMarkdown("include.md"),
            textOutput("omegaCalced"),
            img(src='ellipse_specifications.svg', align = "left"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$omegaCalced <- renderText({
        omega <- calc_omega(input$p, input$q, input$h, input$a, input$b)
        paste("The solid anlge of your ellipse is = ", as.character(omega), " sr.", sep="")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
