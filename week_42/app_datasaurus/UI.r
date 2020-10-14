ui <- fluidPage(
    titlePanel("Datasaurus exploration"),
    sidebarLayout(
		sidebarPanel(
			wellPanel(
			    selectizeInput(
				'dataset', h5('Chose a dataset to display'),choices = list_dataset,selected = 3
				)
			),
			wellPanel(
				checkboxGroupInput("posit", "Position parameters to display", choices = posit_param, selected = "mean")
			),
			wellPanel(
				checkboxGroupInput("disp", "Dispersion parameters to display", choices = disp_param, selected = "SD")
			)
		),
		mainPanel(
			column(
				width = 12, align = "center",
				plotOutput("plot_xy", width = "500px", height = "500px",)
			),
			fluidRow(
				column(
					width = 6,
					wellPanel(
						h4("Position parameters :"),
						h5(tableOutput("posit"))
					)
				),
				column(
					width = 6,
					wellPanel(
						h4("Dispersion parameters :"),
						h5(tableOutput("disp"))
					)
				)
			)
		)
    )
)